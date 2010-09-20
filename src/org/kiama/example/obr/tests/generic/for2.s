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
    ! StW(Local(8),IntDatum(42))
    mov 42, %l1
    st %l1, [%l0+8]
    ! StW(Local(4),IntDatum(20))
    mov 20, %l1
    st %l1, [%l0+4]
    ! StW(Local(12),IntDatum(10))
    mov 10, %l1
    st %l1, [%l0+12]
    ! Bne(CmpgtW(LdW(Local(4)),LdW(Local(12))),L2)
    ld [%l0+4], %l1
    ld [%l0+12], %l2
    cmp %l1, %l2
    mov 1, %l2
    bg L4
    nop
    mov 0, %l2
L4:
    tst %l2
    bne L2
    nop
    ! Jmp(L1)
    ba L1
    nop
    ! LabelDef(L3)
L3:
    ! StW(Local(4),AddW(LdW(Local(4)),IntDatum(1)))
    ld [%l0+4], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+4]
    ! LabelDef(L1)
L1:
    ! StW(Local(8),AddW(LdW(Local(8)),LdW(Local(4))))
    ld [%l0+8], %l1
    ld [%l0+4], %l2
    add %l1, %l2, %l2
    st %l2, [%l0+8]
    ! Bne(CmpltW(LdW(Local(4)),LdW(Local(12))),L3)
    ld [%l0+4], %l1
    ld [%l0+12], %l2
    cmp %l1, %l2
    mov 1, %l2
    bl L5
    nop
    mov 0, %l2
L5:
    tst %l2
    bne L3
    nop
    ! LabelDef(L2)
L2:
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
