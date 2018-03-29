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

(* 
  Integer register map:
    a0       return address
    a1       stack pointer
    a2 - a7  general purpose (preserved on call)
    a8 - a15 general purpose (not preserved)
    
    f0       trap pointer (preserved) 
    f1       allocation pointer (preserved) 
    f2       allocation limit (preserved) 
*)

let int_reg_name =
  [|"a2"; "a3"; "a4"; "a5"; "a6"; "a7"; 
    "a8"; "a9"; "a10"; "a11"; "a12"; "a13"; "a14"; "a15"|]


let num_register_classes = 1

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 0

let num_available_registers = [| 14 |]

let first_available_register = [| 0 |]

let register_name r = assert (r < 14);int_reg_name.(r)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 14 Reg.dummy in
  for i = 0 to 13 do v.(i) <- Reg.at_location Int (Reg i) done; v

let all_phys_regs = hard_int_reg

let phys_reg n = hard_int_reg.(n)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

let loc_spacetime_node_hole = Reg.dummy  (* Spacetime unsupported *)


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

(* 
 * Calling conventions CALL0 ABI
 * a0 Return Address
 * a1 sp (preserved)
 * a2 – a7 Function Arguments
 *)

let loc_arguments arg = 
  let (loc, alignment) = 
    calling_conventions 0 5 outgoing (single_regs arg) 
  in
  ensure_single_regs loc, alignment

let loc_parameters arg = 
  let (loc, _ofs) =
    calling_conventions 0 5 incoming (single_regs arg)
  in 
  ensure_single_regs loc 

let loc_results res =
  let (loc, _ofs) = 
    calling_conventions 0 3 not_supported (single_regs res)
  in
  ensure_single_regs loc 


(* 
 * Calling conventions CALL8 ABI
 * a8 Return Address
 * a9 Callee's stack pointer (set by ENTRY)
 * a10 – a15 Function Arguments
 * Return in a10 – a13
 *)

let loc_external_results res = 
  let (loc, _ofs) = 
    calling_conventions 0 3 not_supported (single_regs res)
  in
  ensure_single_regs loc 

let loc_external_arguments arg = 
  calling_conventions 0 5 outgoing arg 

(* a2 *)
let loc_exn_bucket = phys_reg 0

let regs_are_volatile _rs = false 


let _call12_destroyed = 
  Array.of_list(List.map phys_reg 
    [10; 11; 12; 13])

let _call8_destroyed = 
  Array.of_list(List.map phys_reg 
    [6; 7; 8; 9; 10; 11; 12; 13])

let _call4_destroyed = 
  Array.of_list(List.map phys_reg 
    [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13])

let destroyed_at_oper = function
  | Iop(Icall_ind _ | Icall_imm _) -> all_phys_regs
  | Iop(Iextcall _) -> all_phys_regs
  | Iop(Ialloc _) -> all_phys_regs
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
  | Iextcall _ -> 0
  | Icall_ind _ | Icall_imm _ -> 0
  | Ialloc _ -> 0
  | _ -> 14

let max_register_pressure arg = [| safe_register_pressure arg |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound _) | Iintop_imm(Icheckbound _, _)
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

