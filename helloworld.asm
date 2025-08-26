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

; ===== Code at $C100 =====
* = $C100

; Zero-page placement (your 4 bytes): 32-bit multiplicand shifter
M0 = $FB
M1 = $FC
M2 = $FD
M3 = $FE

CHROUT = $FFD2

start:
    ; --- demo inputs: 12345 * 54321 ---
    lda #<$3039      ; 12345
    sta mulA
    lda #>$3039
    sta mulA+1

    lda #<$D431      ; 54321
    sta mulB
    lda #>$D431
    sta mulB+1

    jsr mul16u       ; (mulA * mulB) -> R3..R0
    jsr print_all   ; print inputs and product
    jmp *           ; stop here


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
