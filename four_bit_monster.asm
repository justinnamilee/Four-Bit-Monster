;;--------------------------------------////////
;;                                     ///////
;;                                      //////
;;      Four-bit Monster               /////
;;                                    ////
;;             {RPN Calculator}        ///
;;                                    //
;;___________________________________/
;;
;;   Author: Justin Lee  <justin at taiz dot me>
;;   License: GNU GPLv3 or above
;;   Copyright: Justin Lee 2011 - 2018
;;
;;   Compiler & Emulator: http://jniosemu.codetouch.com/
;;
;;   Description:
;;       This code describes a four-bit, four register
;;       calculator  based on the JNios-II Emulator.  Even
;;       though there are only four registers; each is
;;       actually 8-bit, meaning there are eight useable
;;       4-bit slots.  The computer has has a data stack
;;       of 32 words, where each word can store up to four
;;       bytes (registers) -- meaning eight nybbles each.
;;       In addition, there is one 32-bit memory value.
;;
;;       There are four buttons and four switches that are
;;       used as input mechanisms.  The first button is
;;       dup (push), it takes the value of the switches and
;;       puts it at the top of the stack.  Second is pop,
;;       it decrements the stack pointer to the previous
;;       slot; it can be thought of as backspace.  The
;;       third is op -- short for    operation, the value
;;       of the switches is decode to be an instruction
;;       with either the last two registers or   simply the
;;       last register being used as the operands.  Last
;;       but not least is the clear button; it is an
;;       alias for   "pop until we're at the first slot,
;;       then pop again", which will run the init code
;;       over again (reseting everything).
;;
;;       Note: For   the time being split-byte registers
;;       are not enabled.  This means that there are only
;;       four registers -- not eight, and that there are
;;       only 32*4 stack slots -- not 32*8.
;;
;;   Operations:
;;
;;       1 PUSH  Store the four bit value of the switches
;;               to the top of the stack.  When the ceiling
;;               of the stack is reached, push will write
;;               over the final slot.
;;
;;
;;       2 POP   Move the stack pointer back one slot.  If
;;               the floor   of the stack is reached, clear
;;               will be called -- reseting everything but
;;               memory.
;;
;;
;;       3 OP    One of sixteen possible operations:
;;
;;           0x0     0000    nop
;;           0x1     0001    add
;;           0x2     0010    sub
;;           0x3     0011    mul
;;           0x4     0100    div
;;           0x5     0101    and
;;           0x6     0110    or
;;           0x7     0111    xor
;;
;;           0x8     1000
;;           0x9     1001
;;           0xA     1010
;;           0xB     1011
;;           0xC     1100
;;           0xD     1101
;;           0xE     1110
;;           0xF     1111
;;
;;
;;       4 CLR   Shortcut for "pop until the floor is
;;               reached" -- this will reset everything
;;               but memory.
;;
;;
;;;;;;;;;;;;;;;;;;
;; TO DO ;;
;;;;;;;;;;;;;;;;;;
;;
;; Implement split-byte register and stack locations.  This means
;; each register could hold two data values, the same applies to
;; the data stack -- effectively doubling all available storage.
;;
;;;;
;;
;; Operations functionality.
;;
;;;;


.global main

.data
    stack:  .word 0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0
            .word 0,0,0,0,0,0,0,0

    memory: .word 0

    reg1:   .byte 0
    reg2:   .byte 0
    reg3:   .byte 0
    reg4:   .byte 0
.text


.equ        CMD_DUP,    0x01
.equ        CMD_POP,    0x02
.equ        CMD_OPR,    0x04
.equ        CMD_CLR,    0x08

.equ        OPR_ADD,    0x01
.equ        OPR_SUB,    0x02
.equ        OPR_MUL,    0x03
;;.equ        OPR_DIV,    0x04
.equ        OPR_AND,    0x05
.equ        OPR_OR,     0x06
.equ        OPR_XOR,    0x07


main:
;; put the stack pointer into r23, clear working registers
    movia   r23,stack
    mov     r6,r0
    mov     r5,r0
    mov     r4,r0
    mov     r3,r0
    mov     r2,r0
;; call this to write the data (zero) out to clear the LEDs
    call    write


loop:
    call    read
;; switch over r3 to read the current button press
;; push the switches onto stack
    andi    r4,r3,CMD_DUP
    bne     r4,r0,calc_dup
;; pop last value onto LEDs / switches
    andi    r4,r3,CMD_POP
    bne     r4,r0,calc_pop
;; do an operation based on the switches
    andi    r4,r3,CMD_OPR
    bne     r4,r0,calc_opr
;; the last is All Clear which resets everthing
    andi    r4,r3,CMD_CLR
    bne     r4,r0,main
;; do it over again
    br      loop


calc_dup:
    movia   r3,memory
    cmpeq   r3,r23,r3
    sub     r23,r23,r3

    call    push

    call    write
    br      loop

calc_pop:
    movia   r3,stack
    beq     r3,r23,main

    call    pop

    call    write
    br      loop

calc_opr:
    beq     r2,r0,loop  ;; nop

    mov     r5,r2

    call    pop
    mov     r3,r2
    call    pop

    movia   r4,add
    andi    r6,r5,OPR_ADD
    bne     r6,r0,_calc_opr

    movia   r4,sub
    andi    r6,r5,OPR_SUB
    bne     r6,r0,_calc_opr

    movia   r4,mul
    andi    r6,r5,OPR_MUL
    bne     r6,r0,_calc_opr

;;    movia   r4
;;    andi    r6,r5,OPR_DIV
;;    bne     r6,r0,_calc_opr

    movia   r4,and
    andi    r6,r5,OPR_AND
    bne     r6,r0,_calc_opr

    movia   r4,or
    andi    r6,r5,OPR_OR
    bne     r6,r0,_calc_opr

    movia   r4,xor
    andi    r6,r5,OPR_XOR
    bne     r6,r0,_calc_opr

_calc_opr:
    callr   r4
    mov     r2,r4
    call    push

    call    write
    br      loop



;;;;;;;;;;
;; operations


;;;; add ;;    R4 <- R2 + R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

add:
    add     r4,r2,r3
    ret


;;;; sub ;;    R4 <- R2 - R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

sub:
    sub     r4,r2,r3
    ret


;;;; mul ;;    R4 <- R2 - R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

mul:
    mul     r4,r2,r3
    ret


;; div ;;    R4 <- R2 / R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

div:
	div		r4,r2,r3
    ret


;;;; and ;;    R4 <- R2 & R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

and:
    addi    sp,sp,-4
    stw     r3,(sp)
    addi    sp,sp,-4
    stw     r3,(sp)

    ldw     r2,(sp)
    addi    sp,sp,4
    ldw     r3,(sp)
    addi    sp,sp,4

    ret


;;;; or ;;     R4 <- R2 | R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

or:
    addi    sp,sp,-4
    stw     r3,(sp)
    addi    sp,sp,-4
    stw     r3,(sp)

    ldw     r2,(sp)
    addi    sp,sp,4
    ldw     r3,(sp)
    addi    sp,sp,4

    ret


;;;; xor ;;    R4 <- R2 ^ R3
;;
;; input:
;;   R2      Operand 1.
;;   R3      Operand 2.
;;
;; output:
;;   R4      Resultand.

xor:
    addi    sp,sp,-4
    stw     r3,(sp)
    addi    sp,sp,-4
    stw     r3,(sp)

    ldw     r2,(sp)
    addi    sp,sp,4
    ldw     r3,(sp)
    addi    sp,sp,4

    ret



;;;;;;;;;;
;; subroutines


;; push & pop ;;;;
;;
;; input:
;;   R2      Register to push on/pop off the stack, 8-bit value.
;;   R23     Current calculator stack pointer.
;;
;; output:
;;   R23     New calculator  stack pointer.

push:
    addi    r23,r23,1
    stb     r2,(r23)

    ret

pop:
    ldb     r2,(r23)
    addi    r23,r23,-1

    ret


;; write ;;;;
;;
;; input:
;;   R2      Value to display on the LEDs.
;;
;; output:
;;   nothing

.equ        LEDDSP_ADDR,    0x00000810

write:
    addi    sp,sp,-4
    stw     r3,(sp)

    movia   r3,LEDDSP_ADDR

    stb     r2,(r3)

    ldw     r3,(sp)
    addi    sp,sp,4

    ret


;; read ;;;;
;;
;; input:
;;   nothing
;;
;; output:
;;   R2      State of switches, 4-bit value.
;;   R3      Button pushed, 4-bit value.

.equ        BUTTON_ADDR,    0x00000840
.equ        SWITCH_ADDR,    0x00000850

read:
    addi    sp,sp,-4
    stw     r5,(sp)             ;; push r5
    addi    sp,sp,-4
    stw     r4,(sp)             ;; push r4

    movia   r5,BUTTON_ADDR
    movia   r2,SWITCH_ADDR

    or      r3,r0,r0            ;; first state (button) is zero

_read:
    ldb     r4,(r5)
    beq     r4,r3,_read
    bne     r3,r0,_read_ret
    or      r3,r0,r4            ;; output state (button)
    br      _read

_read_ret:
    ldb     r2,(r2)             ;; output state (switch)

    ldw     r4,(sp)             ;; pop r4
    addi    sp,sp,4
    ldw     r5,(sp)             ;; pop r5
    addi    sp,sp,4

    ret

