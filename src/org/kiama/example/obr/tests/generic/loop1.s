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
    ! Beq(CmpeqW(LdW(Local(4)),IntDatum(5)),L3)
    ld [%l0+4], %l1
    mov 5, %l2
    cmp %l1, %l2
    mov 1, %l2
    be L5
    nop
    mov 0, %l2
L5:
    tst %l2
    be L3
    nop
    ! Jmp(L2)
    ba L2
    nop
    ! Jmp(L4)
    ba L4
    nop
    ! LabelDef(L3)
L3:
    ! LabelDef(L4)
L4:
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
    ! Write(LdW(Local(4)))
    ld [%l0+4], %l1
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
