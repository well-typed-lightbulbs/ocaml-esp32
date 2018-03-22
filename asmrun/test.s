    l32r    a9, .Ltrap_handler 
    l32i    a8, a10, 0

    .align 4
.Ltrap_handler:
/* Save exception pointer */
    l32r    a11, caml_exception_pointer 
    s32i    a12, a11, 0

