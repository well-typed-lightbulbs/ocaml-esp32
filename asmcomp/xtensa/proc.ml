(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Lucas Pluvinage, OCaml Labs intern, ENS Paris student            *)
(*                                                                        *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Cmm
open Reg
open Mach

(* let fp = Config.with_frame_pointers *)

(* Instruction selection *)

let word_addressed = false

(* 
  Integer register map:
    a0       return address
    a1       stack pointer
    a2 - a7  general purpose (preserved on call)
    a8 - a14 general purpose (not preserved)
    a15      scratch register
  
  Floating point registers (single precision)
    f0       trap pointer (preserved) 
    f1       allocation pointer (preserved) 
    f2       allocation limit (preserved) 
    f8       temp float for neg/abs
*)

(* Registers available for register allocation.
Floating point registers are useless in a floating point computing purpose 
as they are single precision (whereas OCaml uses double precision floats.) *)
(* Maybe we could use them as another class of general purpose registers.*)
(* Moving from general purpose to fp has a latency of 2 instructions cycles *)
let int_reg_name =
  [|"a2"; "a3"; "a4"; "a5"; "a6"; "a7"; 
    "a8"; "a9"; "a10"; "a11"; "a12"; "a13"; "a14"|]

let num_register_classes = 1

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 0

let num_available_registers = [| 13 |]

let first_available_register = [| 0 |]

let register_name r = assert (r < 13);int_reg_name.(r)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)
let hard_int_reg =
  let v = Array.make 13 Reg.dummy in
  for i = 0 to 12 do v.(i) <- Reg.at_location Int (Reg i) done; v

let all_phys_regs = hard_int_reg

let phys_reg n = hard_int_reg.(n)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

let loc_spacetime_node_hole = Reg.dummy  (* Spacetime unsupported *)

(******************** OCaml compilation scheme on ESP32. **********************

Xtensa LX6 processor has 64 registers, but only 16 are visible by standard 
instructions. The "window" of visible registers can be rotated by function
calls and returns, thus acting as a physical stack with overlaps to be able to
pass parameters and return values.

There are mechanisms for automatic spilling when the stack overflows 
(happening generally after a depth of eight C calls) which put the registers in
pre-defined spaces on the stack. This increases efficiency by reducing register
spilling, as long as the program doesn't swing much in call depth. 

C code is compiled using this ABI, but I choose to start with a regular calling
convention for OCaml, as it seems that windowed calling conventions are not 
supported (`loc_results` doesn't take into account the fact that the caller's 
result register is different from the callee's result register.).

As a consequence: 
C is called using windowed calls (CALL4 = rotate window by 4 registers). 
OCaml to OCaml calls are made using CALL0. (no register window rotation).
The weirdnesses in the runtime are mainly to ensure compatibility between the 
two ABIs.
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
          and stack_upper = stack_slot (make_stack (!stack_ofs + 4)) arg1.typ 
          in 
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
 * Calling conventions CALL4 ABI
 * a4 Return Address
 * a5 Callee's stack pointer (set by ENTRY)
 * a6 – a11 Function Arguments
 * Return in a6 – a9
 * a2 and a3 are saved.
 *)
let loc_external_results res = 
  let (loc, _ofs) = 
    calling_conventions 4 7 not_supported (single_regs res)
  in
  ensure_single_regs loc 

let loc_external_arguments arg = 
  calling_conventions 4 9 outgoing arg 

(* a2 *)
let loc_exn_bucket = phys_reg 0

let regs_are_volatile _rs = false 

let call4_destroyed = 
  Array.of_list(List.map phys_reg 
    [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12])

let destroyed_at_oper = function
  | Iop(Icall_ind _ | Icall_imm _)
  | Iop(Iextcall { alloc = true; _}) -> all_phys_regs
  | Iop(Iextcall _) -> call4_destroyed
  | Iop(Ialloc _) -> (* a11-a15 are destroyed.*)
    Array.of_list(List.map phys_reg [9; 10; 11; 12])
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)
let safe_register_pressure = function
  | Iextcall _ -> 0
  | Icall_ind _ | Icall_imm _ -> 0
  | Ialloc _ -> 0
  | _ -> 13

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

