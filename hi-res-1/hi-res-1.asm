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