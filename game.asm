.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 1
BlockRotation: .res 1

.segment "BSS"
BlockGrid: .res 4*4
FieldGrid: .res 10*20
BlockX: .res 1
BlockY: .res 1

.popseg

BlockLocation_X = 88
BlockLocation_Y = 40 - 1
Block_TileId = $10

BoardHeight = 20
BoardWidth  = 10

NmiGame:
    rts

InitGame:
    ; Clear sprites
    jsr ClearSprites

    ldx #0
    jsr FillAttributeTable

    lda #.lobyte(Screen_Playfield)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Playfield)
    sta AddressPointer1+1
    jsr DrawScreen

    lda #.lobyte(Palette_Bg)
    sta AddressPointer1+0
    lda #.hibyte(Palette_Bg)
    sta AddressPointer1+1

    ldx #0
    jsr LoadPalette

    ldx #4
    jsr LoadPalette

    lda #0
    sta CurrentBlock
    lda #0
    sta BlockRotation

    lda #%1000_0000
    sta $2000

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    lda #.lobyte(NmiGame)
    sta NmiHandler+0
    lda #.hibyte(NmiGame)
    sta NmiHandler+1

FrameGame:
    jsr LoadBlock

    jsr ReadControllers

;    lda #BUTTON_START ; start
;    jsr ButtonPressed
;    beq :+
;
;    lda #1
;    sta IsPaused
;    jsr WaitForNMI
;    jmp FramePaused
;:

    lda #BUTTON_A ; A
    jsr ButtonPressed
    beq :+
    inc BlockRotation
    lda #$03
    and BlockRotation
    sta BlockRotation
:

    lda #BUTTON_B ; B
    jsr ButtonPressed
    beq :+
    ;dec BlockRotation
    ;lda #$03
    ;and BlockRotation
    ;sta BlockRotation
    inc CurrentBlock
    lda CurrentBlock
    cmp #7
    bne :+
    lda #0
    sta CurrentBlock
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :+
    dec BlockX
    bpl :+
    lda #0
    sta BlockX
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    inc BlockX
    lda BlockX
    cmp #BoardWidth
    bcc :+
    lda #BoardWidth-1
    sta BlockX
:

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    dec BlockY
    bpl :+
    lda #0
    sta BlockY
:

    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    inc BlockY
    lda BlockY
    cmp #BoardHeight
    bcc :+
    lda #BoardHeight-1
    sta BlockY
:

    jsr UpdateBlock
    jsr WaitForNMI
    jmp FrameGame

;FramePaused:
;    jsr ReadControllers
;
;    lda #BUTTON_START
;    jsr ButtonPressed
;    beq :+
;
;    lda #0
;    sta IsPaused
;    jsr WaitForNMI
;    jmp FrameGame
;:
;
;    jsr WaitForNMI
;    jmp FramePaused

DedFrame:

    jsr ReadControllers
    lda #BUTTON_START
    jsr ButtonPressed
    bne :+
    jsr WaitForNMI
    jmp DedFrame

:   jsr WaitForNMI
    lda #0
    sta $2000
    sta $2001

    jmp InitMenu

; Reads CurrentBlock & CurrentRotation to find the
; correct entry in BlockTiles and loads that data
; into BlockGrid.
LoadBlock:
    lda CurrentBlock
    asl a
    tax

    lda BlockTiles+0, x
    sta AddressPointer1+0
    lda BlockTiles+1, x
    sta AddressPointer1+1

    lda BlockRotation
    asl a
    asl a
    asl a
    asl a
    tay

    ldx #0
@loop:
    lda (AddressPointer1), y
    sta BlockGrid, x
    iny
    inx
    cpx #16
    bne @loop
    rts

UpdateBlock:

    ldx BlockX
    lda BlockGridLocationX, x
    sta TmpX

    ldx BlockY
    lda BlockGridLocationY, x
    sta TmpY

    ldx #0
    ldy #0
@loop:
    lda BlockGrid, x
    beq @next
    clc
    lda BlockSpriteLookupY, x
    adc TmpY
    sta SpriteBlock, y
    iny

    lda #Block_TileId
    sta SpriteBlock, y
    iny

    lda #0
    sta SpriteBlock, y
    iny

    clc
    lda BlockSpriteLookupX, x
    adc TmpX
    sta SpriteBlock, y
    iny

@next:
    inx
    cpx #16
    bne @loop

    rts

BlockSpriteLookupY:
    .byte 0,  0,  0,  0
    .byte 8,  8,  8,  8
    .byte 16, 16, 16, 16
    .byte 24, 24, 24, 24

BlockSpriteLookupX:
    .byte 0, 8, 16, 24
    .byte 0, 8, 16, 24
    .byte 0, 8, 16, 24
    .byte 0, 8, 16, 24

; BlockX/BlockY -> SpriteX/SpriteY for anchor
BlockGridLocationY:
    .repeat 20, i
        .byte BlockLocation_Y+(i*8)
    .endrepeat

BlockGridLocationX:
    .repeat 10, i
        .byte BlockLocation_X+(i*8)
    .endrepeat

; Locations for each tile in a block
BlockTiles:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word :+++++
    .word :++++++
    .word :+++++++

; XX
;  XX
:   .byte 1, 1, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 1, 1, 0, 0
    .byte 1, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 1, 1, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 1, 1, 0, 0
    .byte 1, 0, 0, 0
    .byte 0, 0, 0, 0

;  XX
; XX
:   .byte 0, 1, 1, 0
    .byte 1, 1, 0, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 1, 0, 0, 0
    .byte 1, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 1, 0
    .byte 1, 1, 0, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 1, 0, 0, 0
    .byte 1, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0

;  X
; XXX
:   .byte 0, 1, 0, 0
    .byte 1, 1, 1, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 0, 0
    .byte 1, 1, 1, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 1, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0


; XXX
; X
:   .byte 0, 0, 0, 0
    .byte 1, 1, 1, 0
    .byte 1, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 1, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 1, 0
    .byte 1, 1, 1, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0

; X
; XXX
:   .byte 1, 0, 0, 0
    .byte 1, 1, 1, 0
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 1, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 0, 0
    .byte 1, 1, 1, 0
    .byte 0, 0, 1, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 1, 1, 0, 0
    .byte 0, 0, 0, 0

; XXXX
:   .byte 0, 0, 0, 0
    .byte 1, 1, 1, 1
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0

    .byte 0, 0, 0, 0
    .byte 1, 1, 1, 1
    .byte 0, 0, 0, 0
    .byte 0, 0, 0, 0

    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0
    .byte 0, 1, 0, 0

; XX
; XX
:   .byte 0, 0, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0

    .byte 0, 0, 0, 0
    .byte 0, 1, 1, 0
    .byte 0, 1, 1, 0
    .byte 0, 0, 0, 0

