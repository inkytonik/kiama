    ! Prologue
    .seg "data"
ifmt:
    .asciz "%d"
ofmt:
    .asciz "%d\n"
    .align 4
mem:
    .skip 12
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
    ! Beq(Cond(CmpltW(LdW(Local(0)),IntDatum(0)),IntDatum(1),CmpgtW(LdW(Local(0)),IntDatum(7))),L1)
    ld [%l0], %l1
    mov 0, %l2
    cmp %l1, %l2
    mov 1, %l2
    bl L7
    nop
    mov 0, %l2
L7:
    tst %l2
    be L5
    nop
    mov 1, %l3
    mov %l3, %l2
    ba L6
    nop
L5:
    ld [%l0], %l4
    mov 7, %l5
    cmp %l4, %l5
    mov 1, %l5
    bg L8
    nop
    mov 0, %l5
L8:
    mov %l5, %l2
L6:
    tst %l2
    be L1
    nop
    ! Write(NegW(IntDatum(1)))
    mov 1, %l1
    neg %l1, %l1
    set ofmt, %o0
    mov %l1, %o1
    call printf
    nop
    ! Ret
    ba go
    nop
    ! Jmp(L2)
    ba L2
    nop
    ! LabelDef(L1)
L1:
    ! StW(Local(4),IntDatum(0))
    mov 0, %l1
    st %l1, [%l0+4]
    ! StW(Local(8),IntDatum(1))
    mov 1, %l1
    st %l1, [%l0+8]
    ! Jmp(L3)
    ba L3
    nop
    ! LabelDef(L4)
L4:
    ! StW(Local(4),AddW(LdW(Local(4)),IntDatum(1)))
    ld [%l0+4], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+4]
    ! StW(Local(8),MulW(LdW(Local(8)),LdW(Local(4))))
    ld [%l0+8], %l1
    ld [%l0+4], %l2
    smul %l1, %l2, %l2
    st %l2, [%l0+8]
    ! LabelDef(L3)
L3:
    ! Bne(CmpltW(LdW(Local(4)),LdW(Local(0))),L4)
    ld [%l0+4], %l1
    ld [%l0], %l2
    cmp %l1, %l2
    mov 1, %l2
    bl L9
    nop
    mov 0, %l2
L9:
    tst %l2
    bne L4
    nop
    ! Write(LdW(Local(8)))
    ld [%l0+8], %l1
    set ofmt, %o0
    mov %l1, %o1
    call printf
    nop
    ! Ret
    ba go
    nop
    ! LabelDef(L2)
L2:
    ! Epilogue
go:
    ret
    restore
