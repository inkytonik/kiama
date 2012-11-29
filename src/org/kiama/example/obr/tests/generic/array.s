    ! Prologue
    .seg "data"
ifmt:
    .asciz "%d"
ofmt:
    .asciz "%d\n"
    .align 4
mem:
    .skip 60
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
    ! StW(Local(8),IntDatum(0))
    mov 0, %l1
    st %l1, [%l0+8]
    ! StW(Local(4),IntDatum(1))
    mov 1, %l1
    st %l1, [%l0+4]
    ! StW(Local(52),IntDatum(10))
    mov 10, %l1
    st %l1, [%l0+52]
    ! Bne(CmpgtW(LdW(Local(4)),LdW(Local(52))),L2)
    ld [%l0+4], %l1
    ld [%l0+52], %l2
    cmp %l1, %l2
    mov 1, %l2
    bg L7
    nop
    mov 0, %l2
L7:
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
    ! StW(Indexed(Local(12),MulW(LdW(Local(4)),IntDatum(4))),LdW(Local(4)))
    ld [%l0+4], %l1
    mov 12, %l2
    ld [%l0+4], %l3
    mov 4, %l4
    smul %l3, %l4, %l4
    add %l2, %l4, %l4
    st %l1, [%l0+%l4]
    ! Bne(CmpltW(LdW(Local(4)),LdW(Local(52))),L3)
    ld [%l0+4], %l1
    ld [%l0+52], %l2
    cmp %l1, %l2
    mov 1, %l2
    bl L8
    nop
    mov 0, %l2
L8:
    tst %l2
    bne L3
    nop
    ! LabelDef(L2)
L2:
    ! StW(Local(4),IntDatum(1))
    mov 1, %l1
    st %l1, [%l0+4]
    ! StW(Local(56),IntDatum(10))
    mov 10, %l1
    st %l1, [%l0+56]
    ! Bne(CmpgtW(LdW(Local(4)),LdW(Local(56))),L5)
    ld [%l0+4], %l1
    ld [%l0+56], %l2
    cmp %l1, %l2
    mov 1, %l2
    bg L9
    nop
    mov 0, %l2
L9:
    tst %l2
    bne L5
    nop
    ! Jmp(L4)
    ba L4
    nop
    ! LabelDef(L6)
L6:
    ! StW(Local(4),AddW(LdW(Local(4)),IntDatum(1)))
    ld [%l0+4], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+4]
    ! LabelDef(L4)
L4:
    ! StW(Local(8),AddW(LdW(Local(8)),LdW(Indexed(Local(12),MulW(LdW(Local(4)),IntDatum(4))))))
    ld [%l0+8], %l1
    mov 12, %l2
    ld [%l0+4], %l3
    mov 4, %l4
    smul %l3, %l4, %l4
    add %l2, %l4, %l4
    ld [%l0+%l4], %l5
    add %l1, %l5, %l5
    st %l5, [%l0+8]
    ! Bne(CmpltW(LdW(Local(4)),LdW(Local(56))),L6)
    ld [%l0+4], %l1
    ld [%l0+56], %l2
    cmp %l1, %l2
    mov 1, %l2
    bl L10
    nop
    mov 0, %l2
L10:
    tst %l2
    bne L6
    nop
    ! LabelDef(L5)
L5:
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
