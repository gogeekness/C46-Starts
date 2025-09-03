; ===== BASIC autostart: 10 SYS 49408 (=$C100) =====
* = $0801
!byte $0b,$08,$01,$00,$9e
!text "49408"
!byte 0,0,0

; ===== Data & buffers at $C000 =====
* = $C000

N0:   !byte 0          ; multiplier low
N1:   !byte 0          ; multiplier high
R0:   !byte 0          ; result low
R1:   !byte 0
R2:   !byte 0
R3:   !byte 0          ; result high
mulA: !word 0          ; input A (16-bit, unsigned)
mulB: !word 0          ; input B (16-bit, unsigned)


; --- Grid demo constants (Q3.13 fixed-point) ---
XMIN: !word $C000      ; -2.00 * 8192
XMAX: !word $1000      ; +0.50 * 8192
YMIN: !word $D800      ; -1.25 * 8192
YMAX: !word $2800      ; +1.25 * 8192


; Precomputed steps:
; dx = (XMAX - XMIN)/320 = 20480/320 = 64 = $0040 (exact)
X_STEP_Q: !word $0040
X_STEP_R: !word $0000  ; remainder 0
; dy = (YMAX - YMIN)/200 = 20480/200 = 102 remainder 80 -> $0066, $0050
Y_STEP_Q: !word $0066
Y_STEP_R: !word $0050

; Working vars
x_cur:    !word 0
y_cur:    !word 0
cols_lo:  !byte 0
cols_hi:  !byte 0
accy_lo:  !byte 0      ; Y remainder accumulator (0..199)
accy_hi:  !byte 0

; ===== Code at $C100 =====
* = $C100

; Zero-page placement (your 4 bytes): 32-bit multiplicand shifter
M0 = $FB
M1 = $FC
M2 = $FD
M3 = $FE

CHROUT = $FFD2

start:
    jsr grid_demo     ; set up X/Y fields and iterate 320x200 once
    jmp *             ; stop here    jmp *


; ----------------------------------------------------
; Grid demo: iterate 320x200 over XMIN..XMAX, YMIN..YMAX (Q3.13)
; For each (x,y): signed 16x16 multiply and print in same screen spot
; ----------------------------------------------------
grid_demo:
    ; init Y and remainder
    lda #<YMIN : sta y_cur
    lda #>YMIN : sta y_cur+1
    lda #0     : sta accy_lo : sta accy_hi

    ; rows = 200 ($C8)
    ldy #$C8
.row:
    ; X = XMIN at start of each row
    lda #<XMIN : sta x_cur
    lda #>XMIN : sta x_cur+1

    ; cols = 320 ($0140)
    lda #$40 : sta cols_lo
    lda #$01 : sta cols_hi
.col:
    ; Copy (x_cur,y_cur) to mulA/mulB (signed Q3.13)
    lda x_cur   : sta mulA
    lda x_cur+1 : sta mulA+1
    lda y_cur   : sta mulB
    lda y_cur+1 : sta mulB+1

	; signed multiply
    jsr mul16u       ; (mulA * mulB) -> R3..R0

	; print location
    jsr print_spot   ; print inputs and product

    ; x_cur += X_STEP_Q
    clc
    lda x_cur   : adc X_STEP_Q   : sta x_cur
    lda x_cur+1 : adc X_STEP_Q+1 : sta x_cur+1

    ; dec 16-bit cols (dec the hi and lo bytes seperately)
    dec cols_lo
    bne .col_cont
    dec cols_hi
.col_cont:
    lda cols_lo
    ora cols_hi
    bne .col
    ; End of row: y_cur += Y_STEP_Q

    clc
    lda y_cur   : adc Y_STEP_Q   : sta y_cur
    lda y_cur+1 : adc Y_STEP_Q+1 : sta y_cur+1

    ; accy += Y_STEP_R
    clc
    lda accy_lo : adc #<Y_STEP_R : sta accy_lo
    lda accy_hi : adc #>Y_STEP_R : sta accy_hi

    ; if accy >= 200 ($00C8), subtract 200 and y_cur += 1
    lda accy_hi
    bne .yfix
    lda accy_lo
    cmp #$C8
    bcc .ynofix
.yfix:    
    sec
    lda accy_lo : sbc #$C8 : sta accy_lo
    lda accy_hi : sbc #$00 : sta accy_hi
    inc y_cur
    bne .ynofix
    inc y_cur+1
.ynofix:
    dey
    bne .rows_done
    jmp .row

.rows_done:
	rts

; ----------------------------------------------------
; mul16u: unsigned 16×16 -> 32
;  Inputs:  mulA(16), mulB(16)
;  Output:  R3..R0 (32-bit, low..high)  [R0=low, R3=high]
;  Uses:    M0..M3 ($FB-$FE) as 32-bit shifting multiplicand
;  Clobbers:A, Y, C
; ----------------------------------------------------
mul16u:
    ; init multiplicand shifter M = [mulA | 0000]
    lda mulA     : sta M0
    lda mulA+1   : sta M1
    lda #0       : sta M2 : sta M3

    ; clear result
    lda #0
    sta R0 : sta R1 : sta R2 : sta R3

    ; load multiplier
    lda mulB     : sta N0
    lda mulB+1   : sta N1

    ldy #16
.loop:
    ; test LSB of multiplier (N1:N0)
    lsr N1
    ror N0
    bcc .noadd

    ; R += M (32-bit add)
    clc
    lda R0 : adc M0 : sta R0
    lda R1 : adc M1 : sta R1
    lda R2 : adc M2 : sta R2
    lda R3 : adc M3 : sta R3

.noadd:
    ; M <<= 1 (32-bit)
    asl M0
    rol M1
    rol M2
    rol M3

    dey
    bne .loop
    rts

; ----------------------------------------------------
; Printing helpers (uses KERNAL CHROUT, safe after mul)
; ----------------------------------------------------
print_all:
    ; Print A
    ldx #0
.pa1 lda msgA,x
    beq .pa1done
    jsr CHROUT
    inx
    bne .pa1
.pa1done:
    lda mulA+1
    jsr print_hex_byte
    lda mulA
    jsr print_hex_byte

    ; Print B
    ldx #0
.pb1 lda msgB,x
    beq .pb1done 
    jsr CHROUT
    inx
    bne .pb1
.pb1done:
    lda mulB+1
    jsr print_hex_byte
    lda mulB
    jsr print_hex_byte

    ; Newline
    lda #$0D
    jsr CHROUT

    ; Print Product P
    ldx #0
.pp1 lda msgP,x
    beq .pp1done
    jsr CHROUT
    inx
    bne .pp1
.pp1done:
    lda R3
    jsr print_hex_byte
    lda R2
    jsr print_hex_byte
    lda R1
    jsr print_hex_byte
    lda R0
    jsr print_hex_byte

    ; Newline
    lda #$0D
    jsr CHROUT
    rts

; A in A -> print two hex digits
print_hex_byte:
    pha
    lsr
    lsr
    lsr
    lsr
    jsr print_hex_nibble
    pla
    jsr print_hex_nibble
    rts

; nibble (0..15) in A -> ASCII '0'..'9','A'..'F' via CHROUT
print_hex_nibble:
    and #$0F
    clc
    adc #$30
    cmp #$3A
    bcc .ok
    clc
    adc #$07
.ok jsr CHROUT
    rts

; Zero-terminated labels
msgA: !text "A=$",0
msgB: !text "  B=$",0
msgP: !text "P=$",0
