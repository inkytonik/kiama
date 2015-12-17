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
    ! StW(Local(4),IntDatum(0))
    mov 0, %l1
    st %l1, [%l0+4]
    ! LabelDef(L1)
L1:
    ! StW(Local(4),AddW(LdW(Local(4)),IntDatum(1)))
    ld [%l0+4], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+4]
    ! StW(Local(8),IntDatum(0))
    mov 0, %l1
    st %l1, [%l0+8]
    ! LabelDef(L3)
L3:
    ! StW(Local(8),AddW(LdW(Local(8)),IntDatum(1)))
    ld [%l0+8], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+8]
    ! Beq(CmpeqW(LdW(Local(8)),IntDatum(3)),L5)
    ld [%l0+8], %l1
    mov 3, %l2
    cmp %l1, %l2
    mov 1, %l2
    be L9
    nop
    mov 0, %l2
L9:
    tst %l2
    be L5
    nop
    ! Jmp(L4)
    ba L4
    nop
    ! Jmp(L6)
    ba L6
    nop
    ! LabelDef(L5)
L5:
    ! LabelDef(L6)
L6:
    ! StW(Local(8),AddW(LdW(Local(8)),IntDatum(1)))
    ld [%l0+8], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+8]
    ! Jmp(L3)
    ba L3
    nop
    ! LabelDef(L4)
L4:
    ! Beq(CmpeqW(LdW(Local(4)),IntDatum(5)),L7)
    ld [%l0+4], %l1
    mov 5, %l2
    cmp %l1, %l2
    mov 1, %l2
    be L10
    nop
    mov 0, %l2
L10:
    tst %l2
    be L7
    nop
    ! Jmp(L2)
    ba L2
    nop
    ! Jmp(L8)
    ba L8
    nop
    ! LabelDef(L7)
L7:
    ! LabelDef(L8)
L8:
    ! StW(Local(4),AddW(LdW(Local(4)),IntDatum(1)))
    ld [%l0+4], %l1
    mov 1, %l2
    add %l1, %l2, %l2
    st %l2, [%l0+4]
    ! Jmp(L1)
    ba L1
    nop
    ! LabelDef(L2)
L2:
    ! Write(AddW(LdW(Local(4)),LdW(Local(8))))
    ld [%l0+4], %l1
    ld [%l0+8], %l2
    add %l1, %l2, %l2
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
