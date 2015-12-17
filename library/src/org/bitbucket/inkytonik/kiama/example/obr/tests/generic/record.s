    ! Prologue
    .seg "data"
ifmt:
    .asciz "%d"
ofmt:
    .asciz "%d\n"
    .align 4
mem:
    .skip 16
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
    ! StW(Local(4),IntDatum(9))
    mov 9, %l1
    st %l1, [%l0+4]
    ! StW(Local(8),IntDatum(11))
    mov 11, %l1
    st %l1, [%l0+8]
    ! StW(Local(12),IntDatum(2009))
    mov 2009, %l1
    st %l1, [%l0+12]
    ! Write(LdW(Local(8)))
    ld [%l0+8], %l1
    set ofmt, %o0
    mov %l1, %o1
    call printf
    nop
    ! Ret
    ba go
    nop
    ! Epilogue
go:
    ret
    restore
