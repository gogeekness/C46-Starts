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
; For bitmap Mandelbrot: X range -2.5 to +1.0, Y range -1.25 to +1.25
XMIN: !word $B000      ; -2.5 in Q3.13
XMAX: !word $2000      ; +1.0 in Q3.13  
YMIN: !word $D800      ; -1.25 in Q3.13
YMAX: !word $2800      ; +1.25 in Q3.13

; Escape radius squared = 4.0 in Q3.13 = $8000
ESCAPE_SQ_LO: !byte $00
ESCAPE_SQ_HI: !byte $80

; Maximum iterations (reduced for performance)
MAX_ITER: !byte 16

; Precomputed steps for 256x200 grid:
; dx = (XMAX - XMIN)/256 = 3.5/256 = 0.01367 = 112 = $0070
X_STEP_LO: !byte $70
X_STEP_HI: !byte $00
; dy = (YMAX - YMIN)/200 = 2.5/200 = 0.0125 = 102.4 ≈ 102 = $0066  
Y_STEP_LO: !byte $66
Y_STEP_HI: !byte $00

; Working vars
x_cur_lo:   !byte 0    ; Current c_real low byte
x_cur_hi:   !byte 0    ; Current c_real high byte
y_cur_lo:   !byte 0    ; Current c_imag low byte
y_cur_hi:   !byte 0    ; Current c_imag high byte
pixel_x:    !byte 0    ; Current pixel X (0-255)
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
temp_lo:    !byte 0
temp_hi:    !byte 0

; Graphics variables
current_byte: !byte 0  ; Current byte being built
bit_count:    !byte 0  ; Bits processed in current byte
;screen_lo:    !byte 0  ; Current screen address low
;screen_hi:    !byte 0  ; Current screen address high

; C64 Graphics constants
; BITMAP_BASE = $2000, SCREEN_BASE = $0400

; ===== Code at $C100 =====
* = $C100

; Zero-page placement
M0 = $FB              ; Multiplication workspace
M1 = $FC
M2 = $FD  
M3 = $FE
screen_lo = $B2       ; Screen pointer low byte
screen_hi = $B3       ; Screen pointer high byte
temp_zp = $B4         ; Temporary storage
start:
    jsr setup_bitmap_mode
    jsr mandelbrot_bitmap
    jmp *

; ----------------------------------------------------
; Setup C64 High-Resolution Bitmap Mode
; Sets 320x200 monochrome bitmap graphics
; ----------------------------------------------------
setup_bitmap_mode:
    ; Clear bitmap memory ($2000-$3F3F)
    lda #$00
    sta screen_lo
    lda #$20          ; Start at $2000
    sta screen_hi
    
    ldx #$1F          ; Clear $1F40 bytes ($2000-$3F3F)
    ldy #$40
    
clear_bitmap_loop:
    lda #$00
    sta (<screen_lo),y
    iny
    bne clear_bitmap_loop
    inc screen_hi
    dex
    bpl clear_bitmap_loop

    ; Set up screen RAM for bitmap mode (colors)
    lda #$01          ; White foreground, black background
    ldx #$00
color_loop:
    sta $0400,x       ; Screen RAM
    sta $0500,x
    sta $0600,x
    sta $0700,x
    inx
    bne color_loop

    ; Set VIC-II registers for bitmap mode
    lda #$3B          ; Enable bitmap mode, 25 rows, screen on
    sta $D011
    lda #$C8          ; 40 columns  
    sta $D016
    lda #$18          ; Screen at $0400, bitmap at $2000
    sta $D018

    ; Border and background colors
    lda #$00          ; Black
    sta $D020         ; Border
    sta $D021         ; Background
    rts

; ----------------------------------------------------
; Generate Mandelbrot set and display as bitmap
; Resolution: 256x200 pixels
; ----------------------------------------------------
mandelbrot_bitmap:
    ; Initialize Y coordinate 
    lda #$00          ; YMIN low byte
    sta y_cur_lo
    lda #$D8          ; YMIN high byte  
    sta y_cur_hi
    lda #$00
    sta pixel_y

row_loop:
    ; Initialize X coordinate
    lda #$00          ; XMIN low byte
    sta x_cur_lo
    lda #$B0          ; XMIN high byte
    sta x_cur_hi
    lda #$00
    sta pixel_x
    sta current_byte
    sta bit_count

    ; Calculate screen address for this row
    lda pixel_y
    jsr calc_bitmap_addr

col_loop:
    ; Set up Mandelbrot constants for this pixel
    lda x_cur_lo : sta c_real
    lda x_cur_hi : sta c_real+1
    lda y_cur_lo : sta c_imag
    lda y_cur_hi : sta c_imag+1

    ; Calculate Mandelbrot iteration
    jsr mandelbrot_iterate

    ; Convert iteration count to pixel (0=black, 1=white)
    lda iter_count
    cmp MAX_ITER
    bne pixel_white   ; If not MAX_ITER, escaped = white
    lda #$00          ; In set = black
    jmp set_pixel
pixel_white:
    lda #$01          ; Escaped = white

set_pixel:
    ; Shift pixel into current byte (MSB first)
    asl current_byte
    ora current_byte
    sta current_byte
    inc bit_count

    ; If we have 8 bits, write to screen
    lda bit_count
    cmp #$08
    bne no_write

    ; Write byte to bitmap
    ldy #$00
    lda current_byte
    sta (<screen_lo),y
    
    ; Increment screen address
    inc screen_lo
    bne addr_ok
    inc screen_hi
addr_ok:

    ; Reset bit counter and byte
    lda #$00 
    sta bit_count 
    sta current_byte

no_write:
    ; Move to next X coordinate: x_cur += X_STEP
    clc
    lda x_cur_lo
    adc X_STEP_LO
    sta x_cur_lo
    lda x_cur_hi
    adc X_STEP_HI
    sta x_cur_hi

    ; Next pixel X
    inc pixel_x
    bne col_loop      ; Continue until pixel_x wraps to 0 (256 pixels)

    ; Handle partial byte at end of row if needed
    lda bit_count
    beq no_partial
    
shift_partial_loop:
    asl current_byte
    inc bit_count
    lda bit_count
    cmp #$08
    bne shift_partial_loop
    
    ; Write final byte
    ldy #$00
    lda current_byte
    sta (<screen_lo),y     ; Force zero-page indirect indexed
    inc screen_lo
    bne no_partial
    inc screen_hi

no_partial:
    ; Move to next Y coordinate: y_cur += Y_STEP
    clc
    lda y_cur_lo
    adc Y_STEP_LO
    sta y_cur_lo
    lda y_cur_hi
    adc Y_STEP_HI
    sta y_cur_hi

    ; Next row
    inc pixel_y
    lda pixel_y
    cmp #200          ; Process 200 rows (using 8-bit compare)
    beq mandel_done
    jmp row_loop

mandel_done:
    rts

; ----------------------------------------------------
; Calculate bitmap address for given Y coordinate
; Input: A = Y coordinate (0-199)
; Output: screen_lo/hi = bitmap address for start of row
; ----------------------------------------------------
calc_bitmap_addr:
    ; Save Y coordinate
    sta temp_zp
    
    ; Calculate row address: BITMAP_BASE + (Y/8)*320 + (Y&7)*40
    ; First: Y/8
    lsr                    ; Divide by 8
    lsr
    lsr
    sta temp_lo            ; temp_lo = Y/8
    
    ; Start with base address $2000
    lda #$00
    sta screen_lo
    lda #$20
    sta screen_hi
    
    ; Add 256 * (Y/8)
    lda temp_lo
    clc
    adc screen_hi
    sta screen_hi
    
    ; Add 64 * (Y/8)
    lda temp_lo
    asl                    ; *2
    asl                    ; *4  
    asl                    ; *8
    asl                    ; *16
    asl                    ; *32
    asl                    ; *64
    clc
    adc screen_lo
    sta screen_lo
    bcc no_carry1
    inc screen_hi
no_carry1:
    
    ; Add (Y&7)*40
    lda temp_zp
    and #$07               ; Y&7
    beq no_offset          ; If 0, skip
    
    ; Multiply by 40 = 32 + 8
    sta temp_hi            ; Save Y&7
    asl                    ; *2
    asl                    ; *4
    asl                    ; *8
    sta temp_lo            ; temp_lo = (Y&7)*8
    
    lda temp_hi            ; Get Y&7 again
    asl                    ; *2
    asl                    ; *4
    asl                    ; *8
    asl                    ; *16
    asl                    ; *32
    clc
    adc temp_lo            ; Add (Y&7)*8 to get (Y&7)*40
    
    clc
    adc screen_lo
    sta screen_lo
    bcc no_offset
    inc screen_hi

no_offset:
    rts

; ----------------------------------------------------
; Mandelbrot iteration algorithm
; Input: c_real, c_imag (the complex constant c)
; Output: iter_count (number of iterations before escape)
; ----------------------------------------------------
mandelbrot_iterate:
    ; Initialize z = 0 + 0i
    lda #$00
    sta z_real   
    sta z_real+1
    sta z_imag   
    sta z_imag+1
    sta iter_count

iter_loop:
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
    lda z_real_sq   
    adc z_imag_sq   
    sta temp_lo
    lda z_real_sq+1 
    adc z_imag_sq+1 
    sta temp_hi

    ; Compare with escape radius squared ($8000)
    lda temp_hi
    cmp ESCAPE_SQ_HI
    bcc no_escape    ; |z|^2 < $8000, continue iterating
    bne escaped      ; |z|^2 > $8000, definitely escaped
    lda temp_lo
    cmp ESCAPE_SQ_LO   
    bcs escaped      ; |z|^2 >= $8000, escaped

no_escape:
    ; Check if we've reached maximum iterations
    lda iter_count
    cmp MAX_ITER
    beq max_reached

    ; Calculate new z = z^2 + c
    ; New z_real = z_real^2 - z_imag^2 + c_real
    sec
    lda z_real_sq   
    sbc z_imag_sq   
    sta temp_lo
    lda z_real_sq+1 
    sbc z_imag_sq+1 
    sta temp_hi
    clc  
    lda temp_lo   
    adc c_real   
    sta z_real
    lda temp_hi 
    adc c_real+1 
    sta z_real+1

    ; New z_imag = 2 * z_real_old * z_imag_old + c_imag
    ; Note: We need the OLD z_real, but we just overwrote it!
    ; Let's recalculate: z_real_old = sqrt(z_real_sq)
    ; Actually, let's use a different approach: calculate 2*z_real*z_imag first
    
    ; Get original z_real and z_imag for the cross term
    ; We'll use the fact that we need 2*z_real*z_imag
    lda z_real   : sta mulA
    lda z_real+1 : sta mulA+1
    lda z_imag   : sta mulB
    lda z_imag+1 : sta mulB+1
    jsr mul16s
    
    ; Multiply by 2 (shift left 1) and add c_imag
    asl R1
    rol R2
    clc
    lda R1 
    adc c_imag   
    sta z_imag
    lda R2 
    adc c_imag+1 
    sta z_imag+1

    ; Increment iteration counter and continue
    inc iter_count
    jmp iter_loop

escaped:
max_reached:
    rts

; ----------------------------------------------------
; mul16s: signed 16x16 -> 32 wrapper around mul16u
; ----------------------------------------------------
mul16s:
    lda mulA+1
    eor mulB+1
    and #$80
    sta signflag

    lda mulA+1
    bpl Aok
    lda mulA   : eor #$FF : clc : adc #$01 : sta mulA
    lda mulA+1 : eor #$FF : adc #$00 : sta mulA+1
Aok:
    lda mulB+1
    bpl Bok
    lda mulB   : eor #$FF : clc : adc #$01 : sta mulB
    lda mulB+1 : eor #$FF : adc #$00 : sta mulB+1
Bok:
    jsr mul16u

    lda signflag
    bpl mul_done
    lda R0 : eor #$FF : clc : adc #$01 : sta R0
    lda R1 : eor #$FF : adc #$00 : sta R1
    lda R2 : eor #$FF : adc #$00 : sta R2
    lda R3 : eor #$FF : adc #$00 : sta R3
mul_done:
    rts

; ----------------------------------------------------
; mul16u: unsigned 16×16 -> 32
; ----------------------------------------------------
mul16u:
    sty savY

    lda mulA     : sta M0
    lda mulA+1   : sta M1
    lda #$00     : sta M2 : sta M3

    lda #$00
    sta R0 : sta R1 : sta R2 : sta R3

    lda mulB     : sta N0
    lda mulB+1   : sta N1

    ldy #$10
mul_loop:
    lsr N1
    ror N0
    bcc noadd

    clc
    lda R0 : adc M0 : sta R0
    lda R1 : adc M1 : sta R1
    lda R2 : adc M2 : sta R2
    lda R3 : adc M3 : sta R3

noadd:
    asl M0 
    rol M1
    rol M2
    rol M3

    dey
    bne mul_loop

    ldy savY
    rts