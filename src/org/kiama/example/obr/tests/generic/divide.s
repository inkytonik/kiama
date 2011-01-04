    ! Prologue
    .seg "data"
ifmt:
    .asciz "%d"
ofmt:
    .asciz "%d\n"
    .align 4
mem:
    .skip 4
    .seg "text"
    .globl main
main:
    save %sp, -112, %sp
    set mem, %l0
    ! Read(Local(0))
    set ifmt, %o0
    add %l0, 0, %o1
    call scanf
    nop
    ! Write(DivW(IntDatum(9),IntDatum(2)))
    mov 9, %l1
    mov 2, %l2
    sdiv %l1, %l2, %l2
    set ofmt, %o0
    mov %l2, %o1
    call printf
    nop
    ! Ret
    ba go
    nop
    ! Epilogue
go:
    ret
    restore
