#include "caml/m.h"

    .text
    .globl caml_system__code_begin
caml_system__code_begin:
    
    .align  4
    .globl  caml_call_gc
caml_call_gc:
    /* Record return address */
    l32r    a15, caml_last_return_address
    s32i    a0, a15, 0
.Lcaml_call_gc:
    /* Record lowest stack address */
    l32r    a15, caml_bottom_of_stack
    s32i    sp, a15, 0
    /* Push registers on the stack */
    addi    sp, sp, -4*8
    s32i    a2, sp, 0
    s32i    a3, sp, 4
    s32i    a4, sp, 8
    s32i    a5, sp, 12
    s32i    a6, sp, 16
    s32i    a7, sp, 20
    s32i    a15, sp, 24
    s32i    a0, sp, 28
    /* Save stack pointer */
    l32r    a15, caml_gc_regs
    s32i    sp, a15, 0
    /* Save allocation pointer */
    l32r    a14, caml_young_ptr 
    s32i    a13, a14, 0
    /* Save trap pointer */
    l32r    a15, caml_exception_pointer 
    s32i    a12, a15, 0
    /* Call the garbage collector */
    call0   caml_garbage_collection
    /* Restore registers */
    l32i    a2, sp, 0
    l32i    a3, sp, 4
    l32i    a4, sp, 8
    l32i    a5, sp, 12
    l32i    a6, sp, 16
    l32i    a7, sp, 20
    l32i    a15, sp, 24
    l32i    a0, sp, 28
    addi    sp, sp, 4*8
    /* Reload new allocation pointer and limit */
    l32r    a15, caml_young_limit
    l32i    a13, a14, 0 
    l32i    a14, a15, 0
    /* Return to caller */
    ret 
    .type   caml_call_gc, %function
    .size   caml_call_gc, .-caml_call_gc


    .align  4
    .globl  caml_alloc1
caml_alloc1:
.Lcaml_alloc1:
    addi    a13, a13, -8
    bltu    a13, a14, 1f
    ret 
1:  /* Record return address */
    l32r    a7, caml_last_return_address 
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
1:  /* Record return address */
    l32r    a7, caml_last_return_address 
    s32i    a0, a7, 0
    /* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc2 
    .type   caml_alloc2, %function 
    .size   caml_alloc2, .-caml_alloc2 


    .align  4
    .globl  caml_alloc3
caml_alloc3:
.Lcaml_alloc3:
    addi    a13, a13, -16
    bltu    a13, a14, 1f
    ret 
1:  /* Record return address */
    l32r    a7, caml_last_return_address 
    s32i    a0, a7, 0
    /* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc3 
    .type   caml_alloc3, %function 
    .size   caml_alloc3, .-caml_alloc3 


    .align  4
    .globl  caml_allocN
caml_allocN:
.Lcaml_allocN:
    add     a13, a13, a7
    bltu    a13, a14, 1f
    ret 
1:  /* Record return address */
    l32r    a7, caml_last_return_address 
    s32i    a0, a7, 0
    /* Call GC (preserves a7) */
    call0   .Lcaml_call_gc 
    l32i    a0, a7, 0
    j       .Lcaml_alloc3 
    .type   caml_allocN, %function 
    .size   caml_allocN, .-caml_allocN 


/* Call a C function from OCaml */
/* Function to call is in a8 */
/* a2 to a7 are function arguments */
/* a12 to a15 is callee saved */

    .align  4
    .globl  caml_c_call
caml_c_call:
    /* Record lowest stack address and return address */
    l32r    a9, caml_last_return_address 
    l32r    a10, caml_bottom_of_stack
    s32i    a0, a9, 0 
    s32i    sp, a10, 0
    /* Preserve return address */
    mov     a15, a0 
    /* Make the exception handler alloc ptr available to the C code */
    l32r    a9, caml_young_ptr
    l32r    a10, caml_exception_pointer
    s32i    a13, a9, 0
    s32i    a12, a10, 0
    /* Call the C function */
    call0   a8 
    /* Reload alloc ptr and alloc limit */
    l32r    a10, caml_young_limit
    s32i    a13, a9, 0 /* a9 still points to caml_young_ptr */
    s32i    a14, a10, 0 
    /* Return */
    jx      a15
    .type   caml_c_call, %function
    .size   caml_c_call, .-caml_c_call

