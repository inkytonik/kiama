    ! Prologue
    .seg "data"
ifmt:
    .asciz "%d"
ofmt:
    .asciz "%d\n"
    .align 4
mem:
    .skip 8
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
    ! Read(Local(4))
    set ifmt, %o0
    add %l0, 4, %o1
    call scanf
    nop
    ! Jmp(L1)
    ba L1
    nop
    ! LabelDef(L2)
L2:
    ! Beq(CmpgtW(LdW(Local(0)),LdW(Local(4))),L3)
    ld [%l0], %l1
    ld [%l0+4], %l2
    cmp %l1, %l2
    mov 1, %l2
    bg L5
    nop
    mov 0, %l2
L5:
    tst %l2
    be L3
    nop
    ! StW(Local(0),SubW(LdW(Local(0)),LdW(Local(4))))
    ld [%l0], %l1
    ld [%l0+4], %l2
    sub %l1, %l2, %l2
    st %l2, [%l0]
    ! Jmp(L4)
    ba L4
    nop
    ! LabelDef(L3)
L3:
    ! StW(Local(4),SubW(LdW(Local(4)),LdW(Local(0))))
    ld [%l0+4], %l1
    ld [%l0], %l2
    sub %l1, %l2, %l2
    st %l2, [%l0+4]
    ! LabelDef(L4)
L4:
    ! LabelDef(L1)
L1:
    ! Bne(CmpneW(LdW(Local(0)),LdW(Local(4))),L2)
    ld [%l0], %l1
    ld [%l0+4], %l2
    cmp %l1, %l2
    mov 1, %l2
    bne L6
    nop
    mov 0, %l2
L6:
    tst %l2
    bne L2
    nop
    ! Write(LdW(Local(0)))
    ld [%l0], %l1
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
