(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt            *)
(*                          Bill O'Farrell, IBM                           *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2015 IBM (Bill O'Farrell with help from Tristan Amini).    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the Z Processor *)

open Misc
open Cmm
open Reg
open Mach

(* let fp = Config.with_frame_pointers *)

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

let int_reg_name =
  [| "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "a9"; "a10"; "a11";
     "a12"; "a13"; "a14"; "a15";|]

(* let mac_reg_name =
  [| "m0"; "m1"; "m2"; "m3";|]
*)
(*
let float_reg_name =
  [| "f0"; "f2"; "f4"; "f6"; "f1"; "f3"; "f5"; "f7";
     "f8"; "f9"; "f10"; "f11"; "f12"; "f13"; "f14"; "f15" |]
*)

let num_register_classes = 1

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 0

let num_available_registers = [| 16 |]

let first_available_register = [| 0 |]

let register_name r = int_reg_name.(r)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 16 Reg.dummy in
  for i = 0 to 15 do v.(i) <- Reg.at_location Int (Reg i) done; v

let all_phys_regs = hard_int_reg

let phys_reg n = hard_int_reg.(n)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

let loc_spacetime_node_hole = Reg.dummy  (* Spacetime unsupported *)

(* 
 * Calling conventions
 * Beginning with CALL0 ABI (xtensa.pdf: 613).
 * a0 Return Address
 * a1 (sp) Stack Pointer (callee-saved)
 * a2 – a7 Function Arguments
 * a8 Static Chain (see Section 8.1.8)
 * a12 – a15 Callee-saved
 * a15 Stack-Frame Pointer (optional)
 *)

let calling_conventions
    first_reg last_reg make_stack arg =
  let loc = Array.make (Array.length arg) [| Reg.dummy |] in 
  let current_reg = ref first_reg in  
  let stack_ofs = ref 0 in 
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with 
    | [| arg |] -> 
      begin  
        if !current_reg <= last_reg then begin 
          loc.(i) <- [| phys_reg !current_reg |];
          incr current_reg 
        end else begin 
          loc.(i) <- [| stack_slot (make_stack !stack_ofs) arg.typ |];
          stack_ofs := !stack_ofs + 4
        end;
      end
    | [| arg1; arg2 |] -> 
      begin 
        assert (arg1.typ == arg2.typ);
        current_reg := Misc.align !current_reg 2; (* Two-word align register *)
        if !current_reg + 1 <= last_reg then begin
          let reg_lower = phys_reg !current_reg 
          and reg_upper = phys_reg (!current_reg + 1) in
          loc.(i) <- [| reg_lower; reg_upper |];
          current_reg := !current_reg + 2
        end else begin 
          stack_ofs := Misc.align !stack_ofs 8; (* Two-word align stack *)
          (* TODO: Check arg1.typ == arg2.typ == (Int | Float) *)
          let stack_lower = stack_slot (make_stack !stack_ofs) arg1.typ 
          and stack_upper = stack_slot (make_stack (!stack_ofs + 4)) arg1.typ in 
          loc.(i) <- [| stack_lower; stack_upper |];
          stack_ofs := !stack_ofs + 8
        end
      end
    | _ -> 
      fatal_error "Proc.calling_conventions: bad number of registers for \
        multi-register argument"
  done;
  (loc, Misc.align !stack_ofs 16)

let incoming ofs = Incoming ofs 
let outgoing ofs = Outgoing ofs
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let max_arguments_for_tailcalls = 6

let single_regs arg = Array.map (fun arg -> [| arg |]) arg
let ensure_single_regs res =
  Array.map (function
      | [| res |] -> res
      | _ -> failwith "Proc.ensure_single_regs")
    res

let loc_arguments arg = 
  let (loc, alignment) = 
    calling_conventions 2 7 outgoing (single_regs arg) 
  in
  ensure_single_regs loc, alignment

let loc_parameters arg = 
  let (loc, _ofs) =
    calling_conventions 2 7 incoming (single_regs arg)
  in 
  ensure_single_regs loc 

let loc_results res =
  let (loc, _ofs) = 
    calling_conventions 2 5 not_supported (single_regs res)
  in
  ensure_single_regs loc 


let loc_external_results = loc_results

let loc_external_arguments arg = 
  calling_conventions 2 7 outgoing arg 

let loc_exn_bucket = phys_reg 2

let regs_are_volatile _rs = false 

(* let destroyed_at_c_call =
  Array.of_list(List.map phys_reg 
    [ 0;2;3;4;5;6;7;8;9;10;11;15;
      100;101;102;103;104;105;106;107;
      108;109;110;111;112;113;114;115])


let destroyed_at_alloc = [|phys_reg 2|]
*)

(* TODO: Fix this *)
let destroyed_at_oper = function
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall _ -> 16
  | _ -> 16

let max_register_pressure = function
    Iextcall _ -> [| 16; 16 |]
  | _ -> [| 16; 16 |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound _) | Iintop_imm(Icheckbound _, _) -> false
  | Ispecific _ -> false
  | _ -> true

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()

