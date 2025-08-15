* = $0801                ; BASIC stub: 10 SYS4096
!byte $0b,$08,$01,$00,$9e
!text "4096"
!byte 0,0,0

* = $1000
init_screen:
    lda #$00             ; black
    sta $d020
    sta $d021

    ldx #$00             ; clear screen RAM $0400-$06FF
    lda #$20
clear:
    sta $0400,x
    sta $0500,x
    sta $0600,x
    inx
    bne clear

    lda #%00000001       ; double-size sprite 0
    sta $d01d
    sta $d017

    lda #$0e             ; sprite 0 color = light blue
    sta $d027
    lda #$80             ; $2000 / 64 = $80 -> sprite 0 data pointer
    sta $07f8
    lda #$01             ; enable sprite 0
    sta $d015
    lda #$80
    sta $d000            ; x
    sta $d001            ; y

loop:
    jmp loop

* = $2000                ; sprite data (63 bytes). Use your real data or just zero-fill for now.
!fill 63,0
