open Mach


class scheduler = object

inherit Schedgen.scheduler_generic

(* Latencies (in cycles). Wild guesses.  We multiply all latencies by 2
   to favor dual-issue. *)

method oper_latency = function
    Ireload -> 4
  | Iload(_, _) -> 4
  | Iconst_float _ -> 4 (* turned into a load *)
  | Iintop(Imul) -> 10
  | Iintop_imm(Imul, _) -> 10
  | Iaddf | Isubf | Imulf -> 8
  | Idivf -> 40
  | _ -> 2

method! reload_retaddr_latency = 4

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
  | Ialloc _ -> 4
  | Iintop(Imulh) -> 15
  | Iintop(Idiv|Imod) -> 20
  | Iintop(Icomp _) -> 4
  | Iintop_imm(Icomp _, _) -> 4
  | _ -> 1

method! reload_retaddr_issue_cycles = 1

end

let fundecl f = (new scheduler)#schedule_fundecl f
