; ===== BASIC autostart: 10 SYS 49408 (=$C100) =====
* = $0801
!byte $0b,$08,$01,$00,$9e
!text "49408"
!byte 0,0,0

; ===== Data & buffers at $C000 =====
* = $C000

; Multiplier I/O (kept for reuse)
N0:   !byte 0          ; multiplier low
N1:   !byte 0          ; multiplier high
R0:   !byte 0          ; result low
R1:   !byte 0
R2:   !byte 0
R3:   !byte 0          ; result high
mulA: !word 0          ; input A (16-bit, signed Q3.13)
mulB: !word 0          ; input B (16-bit, signed Q3.13)

; --- Grid demo constants (Q3.13 fixed-point) ---
; Scale = 8192.  -2.00 = $C000, +0.50 = $1000, -1.25 = $D800, +1.25 = $2800
XMIN: !word $C000
XMAX: !word $1000
YMIN: !word $D800
YMAX: !word $2800

; Precomputed steps:
; dx = (XMAX - XMIN)/320 = 20480/320 = 64 = $0040 (exact)
X_STEP_Q: !word $0040
X_STEP_R: !word $0000  ; remainder 0 (unused for X)
; dy = (YMAX - YMIN)/200 = 20480/200 = 102 remainder 80 -> $0066, $0050
Y_STEP_Q: !word $0066
Y_STEP_R: !word $0050

; Working vars (all in $C000 RAM, NOT zero page)
x_cur:      !word 0
y_cur:      !word 0
cols_lo:    !byte 0
cols_hi:    !byte 0
accy_lo:    !byte 0      ; Y remainder accumulator (0..199)
accy_hi:    !byte 0
col_idx_lo: !byte 0
col_idx_hi: !byte 0
rows_left:  !byte 0      ; 200..0
row_idx:    !byte 0      ; 0..199
savY:       !byte 0      ; save/restore Y in mul16u
signflag:   !byte 0
 
; ===== Code at $C100 =====
* = $C100

; Zero-page placement (EXACTLY 4 bytes): 32-bit multiplicand shifter
M0 = $FB
M1 = $FC
M2 = $FD
M3 = $FE

CHROUT = $FFD2

start:
    jsr grid_demo
    jmp *

; ----------------------------------------------------
; Grid demo: iterate 320x200 over XMIN..XMAX, YMIN..YMAX (Q3.13)
; For each (x,y): signed 16x16 multiply and print in same screen spot
; ----------------------------------------------------
grid_demo:
    ; init Y and remainder
    lda #<YMIN : sta y_cur
    lda #>YMIN : sta y_cur+1
    lda #0     : sta accy_lo : sta accy_hi

    lda #$C8   : sta rows_left   ; 200 rows
    lda #$00   : sta row_idx     ; start at row 0

.row:
    ; X = XMIN; col index = 0; columns = 320
    lda #<XMIN : sta x_cur
    lda #>XMIN : sta x_cur+1
    lda #$00   : sta col_idx_lo : sta col_idx_hi
    lda #$40   : sta cols_lo
    lda #$01   : sta cols_hi

.col:
    ; (x_cur,y_cur) -> mulA/mulB (signed Q3.13)
    lda x_cur   : sta mulA
    lda x_cur+1 : sta mulA+1
    lda y_cur   : sta mulB
    lda y_cur+1 : sta mulB+1

    ; signed multiply -> R3..R0
    jsr mul16s

    ; print one spot (overwrite same position)
    jsr print_spot

    ; x_cur += X_STEP_Q
    clc
    lda x_cur   : adc X_STEP_Q   : sta x_cur
    lda x_cur+1 : adc X_STEP_Q+1 : sta x_cur+1

    ; col_idx++ (16-bit)
    inc col_idx_lo
    bne .skip_carry
    inc col_idx_hi
.skip_carry:

    ; columns-- (16-bit)
    lda cols_lo
    bne .dec_lo
    dec cols_hi
.dec_lo:
    dec cols_lo

    ; if columns == 0 -> end of row, else continue columns
    lda cols_lo
    ora cols_hi
    beq .end_row
    jmp .col

.end_row:
    ; y_cur += Y_STEP_Q
    clc
    lda y_cur   : adc Y_STEP_Q     : sta y_cur
    lda y_cur+1 : adc Y_STEP_Q+1   : sta y_cur+1

    ; accy += Y_STEP_R  (FIX: use memory operands, NOT immediate)
    clc
    lda accy_lo : adc Y_STEP_R     : sta accy_lo
    lda accy_hi : adc Y_STEP_R+1   : sta accy_hi

    ; if accy >= 200 ($00C8), subtract 200 and y_cur += 1 LSB
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

    ; rows_left-- ; if 0 -> done
    dec rows_left
    beq .rows_done

    ; next row: row_idx++ and loop
    inc row_idx
    jmp .row

.rows_done:
    rts

; ----------------------------------------------------
; mul16s: signed 16x16 -> 32 wrapper around mul16u
; Inputs: mulA (signed), mulB (signed). Output: R3..R0 (32-bit signed)
; ----------------------------------------------------
mul16s:
    ; sign = sign(A) XOR sign(B)
    lda mulA+1
    eor mulB+1
    and #$80
    sta signflag

    ; abs(A) -> mulA
    lda mulA+1
    bpl .Aok
    lda mulA   : eor #$FF : clc : adc #1 : sta mulA
    lda mulA+1 : eor #$FF : adc #0 : sta mulA+1
.Aok:
    ; abs(B) -> mulB
    lda mulB+1
    bpl .Bok
    lda mulB   : eor #$FF : clc : adc #1 : sta mulB
    lda mulB+1 : eor #$FF : adc #0 : sta mulB+1
.Bok:
    ; unsigned multiply
    jsr mul16u

    ; if negative, negate 32-bit R3..R0
    lda signflag
    bpl .done
    lda R0 : eor #$FF : clc : adc #1 : sta R0
    lda R1 : eor #$FF : adc #0 : sta R1
    lda R2 : eor #$FF : adc #0 : sta R2
    lda R3 : eor #$FF : adc #0 : sta R3
.done:
    rts

; ----------------------------------------------------
; mul16u: unsigned 16×16 -> 32
;  Inputs:  mulA(16), mulB(16)
;  Output:  R3..R0 (32-bit, low..high)  [R0=low, R3=high]
;  Uses:    M0..M3 ($FB-$FE) as 32-bit shifting multiplicand
;  Clobbers:A, Y, C  (Y is preserved across call)
; ----------------------------------------------------
mul16u:
    sty savY                 ; preserve Y for caller

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

    ldy savY                 ; restore Y
    rts

; ----------------------------------------------------
; Print current ROW,COL, X,Y and 32-bit product at a fixed screen position
; Uses CHROUT and HOME ($13) to overwrite same spot
; ----------------------------------------------------
print_spot:
    lda #$13          ; HOME
    jsr CHROUT

    ; ROW (row_idx)
    ldx #0
.pr1 lda msgRow,x : beq .pr1d : jsr CHROUT : inx : bne .pr1
.pr1d:
    lda row_idx : jsr print_hex_byte

    ; COL (col_idx_hi:lo)
    ldx #0
.pr2 lda msgCol,x : beq .pr2d : jsr CHROUT : inx : bne .pr2
.pr2d:
    lda col_idx_hi : jsr print_hex_byte
    lda col_idx_lo : jsr print_hex_byte

    ; newline
    lda #$0D : jsr CHROUT

    ; X
    ldx #0
.psx lda msgX,x : beq .psx1 : jsr CHROUT : inx : bne .psx
.psx1:
    lda x_cur+1 : jsr print_hex_byte
    lda x_cur   : jsr print_hex_byte

    ; space and Y label
    lda #$20 : jsr CHROUT
    ldx #0
.psy lda msgY,x : beq .psy1 : jsr CHROUT : inx : bne .psy
.psy1:
    lda y_cur+1 : jsr print_hex_byte
    lda y_cur   : jsr print_hex_byte

    ; newline and product
    lda #$0D : jsr CHROUT
    ldx #0
.psp lda msgP,x : beq .psp1 : jsr CHROUT : inx : bne .psp
.psp1:
    lda R3 : jsr print_hex_byte
    lda R2 : jsr print_hex_byte
    lda R1 : jsr print_hex_byte
    lda R0 : jsr print_hex_byte

    lda #$0D : jsr CHROUT
    rts

; ----------------------------------------------------
; Hex printing helpers (KERNAL CHROUT)
; ----------------------------------------------------
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

; nibble (0..15) in A -> ASCII '0'..'9','A'..'F'
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
msgRow: !text "ROW=$",0
msgCol: !text " COL=$",0
msgX:   !text "X=$",0
msgY:   !text "Y=$",0
msgP:   !text "P=$",0
