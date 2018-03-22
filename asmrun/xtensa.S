#include "caml/m.h"
    .global caml_garbage_collection
    .literal_position
    
    .text
    .literal_position
    .globl caml_system__code_begin
caml_system__code_begin:
    
    .align  4
    .globl  caml_call_gc
caml_call_gc:
/* Record return address */
    .literal __caml_last_return_address, caml_last_return_address
    l32r    a11, __caml_last_return_address
    l32i    a11, a11, 0
    s32i    a0, a11, 0
.Lcaml_call_gc:
/* Record lowest stack address */
    .literal __caml_bottom_of_stack, caml_bottom_of_stack
    l32r    a11, __caml_bottom_of_stack
    l32i    a11, a11, 0
    s32i    sp, a11, 0
/* Push registers on the stack */
    addi    sp, sp, -4*8
    s32i    a2, sp, 0
    s32i    a3, sp, 4
    s32i    a4, sp, 8
    s32i    a5, sp, 12
    s32i    a6, sp, 16
    s32i    a7, sp, 20
    s32i    a11, sp, 24
    s32i    a0, sp, 28
/* Save stack pointer */
    .literal __caml_gc_regs, caml_gc_regs
    l32r    a11, __caml_gc_regs
    l32i    a11, a11, 0
    s32i    sp, a11, 0
/* Save allocation pointer */
    .literal __caml_young_ptr, caml_young_ptr
    l32r    a14, __caml_young_ptr
    l32i    a14, a14, 0 
    s32i    a13, a14, 0
/* Save trap pointer */
    .literal __caml_exception_pointer, caml_exception_pointer
    l32r    a11, __caml_exception_pointer
    l32i    a11, a11, 0
    s32i    a12, a11, 0
/* Call the garbage collector */
    call0   caml_garbage_collection
/* Restore registers */
    l32i    a2, sp, 0
    l32i    a3, sp, 4
    l32i    a4, sp, 8
    l32i    a5, sp, 12
    l32i    a6, sp, 16
    l32i    a7, sp, 20
    l32i    a11, sp, 24
    l32i    a0, sp, 28
    addi    sp, sp, 4*8
/* Reload new allocation pointer and limit */
    .literal __caml_young_limit, caml_young_limit
    l32r    a11, __caml_young_limit
    l32i    a11, a11, 0
    l32i    a13, a14, 0 
    l32i    a14, a11, 0
/* Return to caller */
    ret 
    .type   caml_call_gc, %function
    .size   caml_call_gc, .-caml_call_gc

    .literal_position
    

    .align  4
    .globl  caml_alloc1
caml_alloc1:
.Lcaml_alloc1:
    addi    a13, a13, -8
    bltu    a13, a14, 1f
    ret 
1: 
/* Record return address */
    l32r    a7, __caml_last_return_address
    l32i    a7, a7, 0 
    s32i    a0, a7, 0
/* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc1 
    .type   caml_alloc1, %function 
    .size   caml_alloc1, .-caml_alloc1 


    .align  4
    .globl  caml_alloc2
caml_alloc2:
.Lcaml_alloc2:
    addi    a13, a13, -12
    bltu    a13, a14, 1f
    ret 
1:  
/* Record return address */
    l32r    a7, __caml_last_return_address
    l32i    a7, a7, 0
    s32i    a0, a7, 0
/* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc2 
    .type   caml_alloc2, %function 
    .size   caml_alloc2, .-caml_alloc2 

    .literal_position
    

    .align  4
    .globl  caml_alloc3
caml_alloc3:
.Lcaml_alloc3:
    addi    a13, a13, -16
    bltu    a13, a14, 1f
    ret 
1:
/* Record return address */
    l32r    a7, __caml_last_return_address
    l32i    a7, a7, 0 
    s32i    a0, a7, 0
/* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc3 
    .type   caml_alloc3, %function 
    .size   caml_alloc3, .-caml_alloc3 

    .literal_position
    

    .align  4
    .globl  caml_allocN
caml_allocN:
.Lcaml_allocN:
    add     a13, a13, a7
    bltu    a13, a14, 1f
    ret 
1: 
/* Record return address */
    l32r    a7, __caml_last_return_address
    l32i    a7, a7, 0
    s32i    a0, a7, 0
/* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc3 
    .type   caml_allocN, %function 
    .size   caml_allocN, .-caml_allocN 


    .literal_position
    
/* Call a C function from OCaml */
/* Function to call is in a8 */
/* a2 to a7 are function arguments */
/* a12 to a15 is callee saved */

    .align  4
    .globl  caml_c_call
caml_c_call:
/* Record lowest stack address and return address */
    l32r    a9, __caml_last_return_address
    l32i    a9, a9, 0
        l32r    a10, __caml_bottom_of_stack
    l32i    a10, a10, 0
    s32i    a0, a9, 0 
    s32i    sp, a10, 0
/* Preserve return address in a15 which is callee saved */
    mov     a15, a0 
/* Make the exception handler alloc ptr available to the C code */
    l32r    a9, __caml_young_ptr
    l32i    a9, a9, 0
    l32r    a10, __caml_exception_pointer
    l32i    a10, a10, 0
    s32i    a13, a9, 0
    s32i    a12, a10, 0
/* Call the C function */
    callx0  a8 
/* Reload alloc ptr and alloc limit */
    l32r    a10, __caml_young_limit
    l32i    a10, a10, 0
    s32i    a13, a9, 0 /* a9 still points to caml_young_ptr */
    s32i    a14, a10, 0 
/* Return */
    jx      a15
    .type   caml_c_call, %function
    .size   caml_c_call, .-caml_c_call

    .literal_position
    
/* Start the OCalm program */

    .align  4
    .globl  caml_start_program
caml_start_program:
    l32r    a11, caml_program 

/* Code shared with caml_callback* */
/* Address of OCaml code to call is in a11 */
/* Arguments to the OCaml code are in a2...a7 */

.Ljump_to_caml:
/* Save return address and callee-save registers */
    addi    sp, sp, -4*6 /* 8-byte aligned */
    s32i    a12, sp, 0
    s32i    a13, sp, 4
    s32i    a14, sp, 8
    s32i    a15, sp, 12
    s32i    a0, sp,  16
/* Setup a callback link on the stack */
    addi    sp, sp, -4*4
    l32r    a8, __caml_bottom_of_stack
    l32i    a8, a8, 0 
    l32r    a9, __caml_last_return_address
    l32i    a9, a9, 0 
    l32r    a10, __caml_gc_regs
    l32i    a10, a10, 0 
    l32i    a8, a8, 0
    l32i    a9, a9, 0
    l32i    a10, a10, 0 
    s32i    a8, sp, 0
    s32i    a9, sp, 4
    s32i    a10, sp, 8
/* Setup a trap frame to catch exceptions escaping the OCaml code */
    addi    sp, sp, -4*2
    l32r    a10, __caml_exception_pointer
    l32i    a10, a10, 0
    .literal __trap_handler, trap_handler
    l32r    a9, __trap_handler
    l32i    a9, a9, 0 
    l32i    a8, a10, 0
    s32i    a8, sp, 0
    s32i    a9, sp, 4
    mov     a12, sp /* Trap pointer */
/* Reload allocation pointers */
    l32r    a8, __caml_young_ptr
    l32i    a8, a8, 0 
    l32i    a13, a8, 0
    l32r    a8, __caml_young_limit
    l32i    a8, a8, 0
    l32i    a14, a8, 0
/* Call the OCaml code */
    callx0  a11
.Lcaml_retaddr:
/* Pop the trap frame, restoring caml_exception_pointer */
    l32r    a8, __caml_exception_pointer
    l32i    a8, a8, 0
    l32i    a9, sp, 0
    s32i    a9, a8, 0
    addi    sp, sp, 8
/* Pop the callback link, restoring the global variables */
.Lreturn_result:
    l32r    a8, __caml_bottom_of_stack
    l32i    a8, a8, 0
    l32i    a9, sp, 0
    s32i    a8, a8, 0
    l32r    a9, __caml_last_return_address
    l32i    a9, a9, 0
    l32i    a8, sp, 4
    s32i    a9, a8, 0
    l32r    a8, __caml_gc_regs
    l32i    a8, a8, 0
    l32i    a9, sp, 8
    s32i    a9, a8, 0
    addi    sp, sp, 16
/* Update allocation pointer */
    l32r    a8, __caml_young_ptr
    l32i    a8, a8, 0 
    s32i    a13, a8, 0
/* Reload callee-save registers and return address */
    l32i    a12, sp, 0
    l32i    a13, sp, 4
    l32i    a14, sp, 8
    l32i    a15, sp, 12
    l32i    a0, sp,  16
    addi    sp, sp, 4*6
    ret 
    .type   .Lcaml_retaddr, %function
    .size   .Lcaml_retaddr, .-.Lcaml_retaddr
    .type   caml_start_program, %function
    .size   caml_start_program, .-caml_start_program

    .literal_position
    

    .align 4
    .globl trap_handler
trap_handler:
/* Save exception pointer */
    l32r    a11, __caml_exception_pointer
    l32i    a11, a11, 0 
    s32i    a12, a11, 0
/* Encode exception bucket as an exception result */
    movi    a11, 2
    or      a2, a2, a11
/* Return it */
    j       .Lreturn_result 
    .type   trap_handler, %function
    .size   trap_handler, .-trap_handler

    .literal_position
    

/* Raise an exception from OCaml */
    
    .align  4
    .globl  caml_raise_exn
caml_raise_exn: 
/* Test if backtrace is active */
    .literal __caml_backtrace_active, caml_backtrace_active
    l32r    a3, __caml_backtrace_active
    l32i    a3, a3, 0 
    l32i    a3, a3, 0
    beqz    a3, 1f
/* Preserve exception bucket in callee-save register a15 */
    mov     a15, a2 
/* Stash the backtrace */
    mov     a3, a0 
    mov     a4, sp 
    mov     a5, a12 
    call0   caml_stash_backtrace
/* Restore exception bucket */
    mov     a2, a15 
1:
/* Cut stack at current trap handler */
    mov     sp, a12
/* Pop previous handler and address of trap, and jump to it */
    l32i    a0, sp, 0
    l32i    a12, sp, 4
    addi    sp, sp, -8
    ret 

    .literal_position
    

/* Raise an exception from C */

    .align 4
    .globl caml_raise_exception 
caml_raise_exception:
/* Reload trap ptr, alloc ptr and alloc limit */
    l32r    a12, __caml_exception_pointer
    l32i    a12, a12, 0 
    l32r    a13, __caml_young_ptr
    l32i    a13, a13, 0 
    l32r    a14, __caml_young_limit
    l32i    a14, a14, 0 
    l32i    a12, a12, 0
    l32i    a13, a13, 0
    l32i    a14, a14, 0
/* Test if backtrace is active */
    l32r    a3, __caml_backtrace_active
    l32i    a3, a3, 0 
    l32i    a3, a3, 0
    beqz    a3, 1f
/* Preserve exception bucket in callee-save register a15 */
    mov     a15, a2 
    l32r    a3, __caml_last_return_address
    l32i    a3, a3, 0 
    l32i    a3, a3, 0
    l32r    a4, __caml_bottom_of_stack
    l32i    a4, a4, 0 
    l32i    a4, a4, 0
    mov     a5, a12 
    call0   caml_stash_backtrace
/* Restore exception bucket */
    mov     a2, a15 
/* Cut stack at current trap handler */
1:
    mov     sp, a12
/* Pop previous handler and address of trap, and jump to it */
    l32i    a0, sp, 0
    l32i    a12, sp, 4
    addi    sp, sp, -8
    ret 
    .type   caml_raise_exception, %function
    .size   caml_raise_exception, .-caml_raise_exception


/* Callback from C to OCaml */

    .align  4
    .globl  caml_callback_exn  
caml_callback_exn:
/* Initial shuffling of argments (a2 = closure, a3 = first arg) */
    mov     a11, a2 
    mov     a2, a3      /* a2 = first arg */
    mov     a3, a11     /* a3 = closure environment */
    l32i    a11, a11, 0 /* code pointer */
    j       .Ljump_to_caml
    .type   caml_callback_exn, %function
    .size   caml_callback_exn, .-caml_callback_exn

    .literal_position
    
    .align  4
    .globl  caml_callback2_exn  
caml_callback2_exn:
/* Initial shuffling of argments (a2 = closure, a3 = arg1, a4 = arg2) */
    mov     a11, a2 
    mov     a2, a3      /* a2 = first arg */
    mov     a3, a4      /* a3 = second arg */
    mov     a4, a11     /* a4 = closure environment */
    l32i    a11, a11, 0 /* code pointer */
    j       .Ljump_to_caml
    .type   caml_callback2_exn, %function
    .size   caml_callback2_exn, .-caml_callback2_exn

    .literal_position
    
    .align  4
    .globl  caml_callback3_exn  
caml_callback3_exn:
/* Initial shuffling of argments (a2 = closure, a3 = arg1, a4 = arg2, a5 = arg3) */
    mov     a11, a2 
    mov     a2, a3      /* a2 = first arg */
    mov     a3, a4      /* a3 = first arg */
    mov     a4, a5      /* a4 = first arg */
    mov     a5, a11     /* a5 = closure environment */
    l32i    a11, a11, 0 /* code pointer */
    j       .Ljump_to_caml
    .type   caml_callback3_exn, %function
    .size   caml_callback3_exn, .-caml_callback3_exn

    .literal_position
    

    .align  4
    .globl  caml_ml_array_bound_error
caml_ml_array_bound_error:
/* Load address of [caml_array_bound_error] in */ 
    .literal __caml_array_bound_error, caml_array_bound_error
    l32r    a8, __caml_array_bound_error
    l32i    a8, a8, 0 
/* Call that function */
    j       caml_c_call 
    .type   caml_ml_array_bound_error, %function
    .size   caml_ml_array_bound_error, .-caml_ml_array_bound_error

    .literal_position

    .globl  caml_system__code_end
caml_system__code_end:

/* GC roots for callback */

    .data
    .align  4
    .globl  caml_system__frametable
caml_system__frametable:
    .word   1               /* one descriptor */
    .word   .Lcaml_retaddr  /* return address into callback */
    .short  -1              /* negative frame size => use callback link */
    .short  0               /* no roots */
    .align  2
    .type   caml_system__frametable, %object
    .size   caml_system__frametable, .-caml_system__frametable

/* Mark stack as non-executable */
    .section .note.GNU-stack,"",%progbits
