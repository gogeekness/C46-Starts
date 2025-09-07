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

; --- Mandelbrot constants (Q3.13 fixed-point) ---
; For 320x200 Mandelbrot: X range -2.5 to +1.0, Y range -1.25 to +1.25
XMIN: !word $B000      ; -2.5 in Q3.13
XMAX: !word $2000      ; +1.0 in Q3.13  
YMIN: !word $D800      ; -1.25 in Q3.13
YMAX: !word $2800      ; +1.25 in Q3.13

; Escape radius squared = 4.0 in Q3.13 = $8000
ESCAPE_SQ: !word $8000

; Maximum iterations (reduced for performance)
MAX_ITER: !byte 16

; Precomputed steps for 320x200 grid:
; dx = (XMAX - XMIN)/320 = 3.5/320 = 28.672 ≈ 29 = $001D
X_STEP_Q: !word $001D
; dy = (YMAX - YMIN)/200 = 2.5/200 = 20.48 ≈ 20 = $0014  
Y_STEP_Q: !word $0014

; Working vars
x_cur:      !word 0    ; Current c_real (x-coordinate)
y_cur:      !word 0    ; Current c_imag (y-coordinate)
pixel_x:    !byte 0    ; Current pixel X (0-319, but we'll use 0-255)
pixel_y:    !byte 0    ; Current pixel Y (0-199)
savY:       !byte 0
signflag:   !byte 0

; Mandelbrot iteration variables
z_real:     !word 0
z_imag:     !word 0
z_real_sq:  !word 0
z_imag_sq:  !word 0
c_real:     !word 0
c_imag:     !word 0
iter_count: !byte 0
temp_word:  !word 0

; Graphics variables
current_byte: !byte 0  ; Current byte being built
bit_count:    !byte 0  ; Bits processed in current byte
screen_addr:  !word 0  ; Current screen address

; C64 Graphics constants
BITMAP_BASE = $2000    ; Bitmap data starts at $2000
SCREEN_BASE = $0400    ; Screen RAM for color info
COLOR_BASE  = $D800    ; Color RAM

; ===== Code at $C100 =====
* = $C100

; Zero-page placement
M0 = $FB
M1 = $FC
M2 = $FD
M3 = $FE

start:
    jsr setup_bitmap_mode
    jsr mandelbrot_bitmap
    jmp *

; ----------------------------------------------------
; Setup C64 High-Resolution Bitmap Mode
; Sets 320x200 monochrome bitmap graphics
; ----------------------------------------------------
setup_bitmap_mode:
    ; Clear bitmap memory ($2000-$3F40)
    lda #0
    tax
    ldy #$20          ; Start at $2000
    sty screen_addr+1
    sta screen_addr

.clear_bitmap:
    sta (screen_addr),y
    iny
    bne .clear_bitmap
    inc screen_addr+1
    lda screen_addr+1
    cmp #$40          ; Stop at $4000
    bne .clear_bitmap

    ; Set up screen RAM for bitmap mode (colors)
    lda #$01          ; White on black
    ldx #0
.color_loop:
    sta SCREEN_BASE,x
    sta SCREEN_BASE+$100,x
    sta SCREEN_BASE+$200,x
    sta SCREEN_BASE+$300,x
    inx
    bne .color_loop

    ; Set VIC-II registers for bitmap mode
    lda #$3B          ; Enable bitmap mode, 25 rows, screen on
    sta $D011
    lda #$C8          ; 40 columns
    sta $D016
    lda #$18          ; Screen at $0400, bitmap at $2000
    sta $D018

    ; Border and background colors
    lda #0            ; Black
    sta $D020         ; Border
    sta $D021         ; Background
    rts

; ----------------------------------------------------
; Generate Mandelbrot set and display as bitmap
; Reduced resolution: 256x200 for simplicity
; ----------------------------------------------------
mandelbrot_bitmap:
    ; Initialize Y coordinate and pixel position
    lda #<YMIN : sta y_cur
    lda #>YMIN : sta y_cur+1
    lda #0     : sta pixel_y

.row_loop:
    ; Initialize X coordinate and pixel position  
    lda #<XMIN : sta x_cur
    lda #>XMIN : sta x_cur+1
    lda #0     : sta pixel_x
    lda #0     : sta current_byte
    lda #0     : sta bit_count

    ; Calculate screen address for this row
    lda pixel_y
    jsr calc_bitmap_addr

.col_loop:
    ; Set up Mandelbrot constants for this pixel
    lda x_cur   : sta c_real
    lda x_cur+1 : sta c_real+1
    lda y_cur   : sta c_imag
    lda y_cur+1 : sta c_imag+1

    ; Calculate Mandelbrot iteration
    jsr mandelbrot_iterate

    ; Convert iteration count to pixel (0=black, >0=white)
    lda iter_count
    cmp MAX_ITER
    beq .pixel_black  ; If MAX_ITER, point is in set = black
    lda #1            ; Otherwise = white
    jmp .set_pixel

.pixel_black:
    lda #0

.set_pixel:
    ; Shift pixel into current byte
    asl current_byte
    ora current_byte
    sta current_byte
    inc bit_count

    ; If we have 8 bits, write to screen
    lda bit_count
    cmp #8
    bne .no_write

    ; Write byte to bitmap
    ldy #0
    lda current_byte
    sta (screen_addr),y
    
    ; Increment screen address
    inc screen_addr
    bne .addr_ok
    inc screen_addr+1
.addr_ok:

    ; Reset bit counter and byte
    lda #0 : sta bit_count : sta current_byte

.no_write:
    ; Move to next X coordinate
    clc
    lda x_cur   : adc X_STEP_Q   : sta x_cur
    lda x_cur+1 : adc X_STEP_Q+1 : sta x_cur+1

    ; Next pixel X
    inc pixel_x
    lda pixel_x
    cmp #$00         ; Process 256 pixels per row
    bne .col_loop

    ; Handle partial byte at end of row
    lda bit_count
    beq .no_partial
    
    ; Shift remaining bits to left
.shift_partial:
    asl current_byte
    inc bit_count
    lda bit_count
    cmp #8
    bne .shift_partial
    
    ; Write final byte
    ldy #0
    lda current_byte
    sta (screen_addr),y
    inc screen_addr
    bne .no_partial
    inc screen_addr+1

.no_partial:
    ; Move to next Y coordinate
    clc
    lda y_cur   : adc Y_STEP_Q     : sta y_cur
    lda y_cur+1 : adc Y_STEP_Q+1   : sta y_cur+1

    ; Next row
    inc pixel_y
    lda pixel_y
    cmp #200          ; Process 200 rows
    beq .done
    jmp .row_loop

.done:
    rts

; ----------------------------------------------------
; Calculate bitmap address for given Y coordinate
; Input: A = Y coordinate (0-199)
; Output: screen_addr = bitmap address for start of row
; ----------------------------------------------------
calc_bitmap_addr:
    ; Bitmap layout: 8x8 char cells, 40 chars wide
    ; Address = BITMAP_BASE + (Y/8)*320 + (Y%8)*40
    
    pha                    ; Save Y
    lsr                    ; Y/8
    lsr
    lsr
    tax                    ; X = Y/8
    
    ; Multiply by 320 (256 + 64) = 256*X + 64*X
    lda #0
    sta screen_addr        ; Clear low byte
    txa
    sta screen_addr+1      ; High byte = X (multiply by 256)
    
    ; Add 64*X
    txa
    asl                    ; X*2
    asl                    ; X*4  
    asl                    ; X*8
    asl                    ; X*16
    asl                    ; X*32
    asl                    ; X*64
    clc
    adc screen_addr
    sta screen_addr
    bcc .no_carry1
    inc screen_addr+1
.no_carry1:
    
    ; Add (Y%8)*40
    pla                    ; Restore Y
    and #7                 ; Y%8
    beq .no_offset         ; If 0, skip
    
    ; Multiply by 40
    sta temp_word
    asl                    ; *2
    asl                    ; *4
    adc temp_word          ; *5
    asl                    ; *10
    asl                    ; *20
    asl                    ; *40
    
    clc
    adc screen_addr
    sta screen_addr
    bcc .no_carry2
    inc screen_addr+1
.no_carry2:

.no_offset:
    ; Add bitmap base address
    clc
    lda screen_addr
    adc #<BITMAP_BASE
    sta screen_addr
    lda screen_addr+1  
    adc #>BITMAP_BASE
    sta screen_addr+1
    rts

; ----------------------------------------------------
; Mandelbrot iteration algorithm (SAME AS BEFORE)
; ----------------------------------------------------
mandelbrot_iterate:
    ; Initialize z = 0 + 0i
    lda #0
    sta z_real   : sta z_real+1
    sta z_imag   : sta z_imag+1
    sta iter_count

.iter_loop:
    ; Calculate z_real^2
    lda z_real   : sta mulA
    lda z_real+1 : sta mulA+1
    lda z_real   : sta mulB  
    lda z_real+1 : sta mulB+1
    jsr mul16s
    lda R1 : sta z_real_sq
    lda R2 : sta z_real_sq+1

    ; Calculate z_imag^2
    lda z_imag   : sta mulA
    lda z_imag+1 : sta mulA+1
    lda z_imag   : sta mulB
    lda z_imag+1 : sta mulB+1  
    jsr mul16s
    lda R1 : sta z_imag_sq
    lda R2 : sta z_imag_sq+1

    ; Calculate |z|^2 = z_real^2 + z_imag^2
    clc
    lda z_real_sq   : adc z_imag_sq   : sta temp_word
    lda z_real_sq+1 : adc z_imag_sq+1 : sta temp_word+1

    ; Compare with escape radius squared
    lda temp_word+1
    cmp #>ESCAPE_SQ
    bcc .no_escape
    bne .escaped
    lda temp_word
    cmp #<ESCAPE_SQ   
    bcs .escaped

.no_escape:
    ; Check max iterations
    lda iter_count
    cmp MAX_ITER
    beq .max_reached

    ; Calculate new z = z^2 + c
    ; z_real = z_real^2 - z_imag^2 + c_real
    sec
    lda z_real_sq   : sbc z_imag_sq   : sta temp_word
    lda z_real_sq+1 : sbc z_imag_sq+1 : sta temp_word+1
    clc  
    lda temp_word   : adc c_real   : sta z_real
    lda temp_word+1 : adc c_real+1 : sta z_real+1

    ; z_imag = 2 * z_real * z_imag + c_imag
    lda z_real   : sta mulA
    lda z_real+1 : sta mulA+1
    lda z_imag   : sta mulB
    lda z_imag+1 : sta mulB+1
    jsr mul16s
    ; Multiply by 2 and add c_imag
    asl R1
    rol R2
    clc
    lda R1 : adc c_imag   : sta z_imag
    lda R2 : adc c_imag+1 : sta z_imag+1

    inc iter_count
    jmp .iter_loop

.escaped:
.max_reached:
    rts

; ----------------------------------------------------
; MULTIPLICATION ROUTINES (UNCHANGED)
; ----------------------------------------------------
mul16s:
    lda mulA+1
    eor mulB+1
    and #$80
    sta signflag

    lda mulA+1
    bpl .Aok
    lda mulA   : eor #$FF : clc : adc #1 : sta mulA
    lda mulA+1 : eor #$FF : adc #0 : sta mulA+1
.Aok:
    lda mulB+1
    bpl .Bok
    lda mulB   : eor #$FF : clc : adc #1 : sta mulB
    lda mulB+1 : eor #$FF : adc #0 : sta mulB+1
.Bok:
    jsr mul16u

    lda signflag
    bpl .done
    lda R0 : eor #$FF : clc : adc #1 : sta R0
    lda R1 : eor #$FF : adc #0 : sta R1
    lda R2 : eor #$FF : adc #0 : sta R2
    lda R3 : eor #$FF : adc #0 : sta R3
.done:
    rts

mul16u:
    sty savY

    lda mulA     : sta M0
    lda mulA+1   : sta M1
    lda #0       : sta M2 : sta M3

    lda #0
    sta R0 : sta R1 : sta R2 : sta R3

    lda mulB     : sta N0
    lda mulB+1   : sta N1

    ldy #16
.loop:
    lsr N1
    ror N0
    bcc .noadd

    clc
    lda R0 : adc M0 : sta R0
    lda R1 : adc M1 : sta R1
    lda R2 : adc M2 : sta R2
    lda R3 : adc M3 : sta R3

.noadd:
    asl M0 
    rol M1
    rol M2
    rol M3

    dey
    bne .loop

    ldy savY
    rts