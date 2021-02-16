    .text

    .global call_gen_code
    .type   call_gen_code, %function
    .align  4
call_gen_code:
    entry   sp, 24
    s32i    a0, sp, 0
# a2 is OCaml function to call
# a3, a4, a5 are arguments 1, 2, 3
    mov     a6, a2
    mov     a2, a3
    mov     a3, a4
    mov     a4, a5
    callx0  a6
    l32i    a0, sp, 0
    retw

    .global caml_c_call
    .type   caml_c_call, %function
    .align  4
caml_c_call:
# function to call is in a9
    jx       a9
