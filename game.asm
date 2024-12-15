.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 1
BlockRotation: .res 1

FieldGrid: .res 10*20

.segment "BSS"
BlockGrid: .res 4*4
BlockX: .res 1
BlockY: .res 1

.popseg

BlockLocation_X = 88
BlockLocation_Y = 40 - 1
Block_TileId = $10

BoardHeight = 20
BoardWidth  = 10

TILE_X = $00
TILE_A = $10
TILE_B = $11
TILE_C = $12

; Column offset for bounding box
BLOCK_START_X = 4

NmiGame:
    rts

InitGame:
    ; Clear sprites
    jsr ClearSprites

    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    .repeat 4*8, i
        lda Palettes+i
        sta $2007
    .endrepeat

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

    lda #$11
    sta FieldGrid+(BoardWidth*0)+0
    sta FieldGrid+(BoardWidth*(BoardHeight-1))+0
    sta FieldGrid+(BoardWidth*0)+9
    sta FieldGrid+(BoardWidth*(BoardHeight-1))+9

    jsr DrawFullBoard_SPEED

    lda #%1000_0000
    sta $2000

    lda #$00
    sta $2003
    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    ;lda #.lobyte(NmiGame)
    ;sta NmiHandler+0
    ;lda #.hibyte(NmiGame)
    ;sta NmiHandler+1

    lda #.lobyte(DrawFullBoard_SPEED)
    sta NmiHandler+0
    lda #.hibyte(DrawFullBoard_SPEED)
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

    lda #BUTTON_START ; start
    jsr ButtonPressed
    beq :+
    jsr PlaceBlock
:

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
    ;inc CurrentBlock
    ;lda CurrentBlock
    ;cmp #7
    ;bne :+
    ;lda #0
    ;sta CurrentBlock
    jsr NextBlock
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

NextBlock:
    inc CurrentBlock
    lda CurrentBlock
    cmp #7
    bne :+
    lda #0
    sta CurrentBlock
:
    tay

    lda #BLOCK_START_X
    sta BlockX

    lda BlockStart_Y, y
    sta BlockY

    lda #0
    sta BlockRotation

    jmp LoadBlock
    ;rts

PlaceBlock:
    ldy BlockY
    ldx BlockToPlayfield_Lo, y
    dex
    txa
    clc
    adc BlockX
    sta AddressPointer1+0

    lda BlockToPlayfield_Hi, y
    adc #0
    sta AddressPointer1+1

    sec
    lda AddressPointer1+0
    sbc #BoardWidth
    sta AddressPointer1+0

    lda AddressPointer1+1
    sbc #0
    sta AddressPointer1+1

    ldy #0
    ldx #0
@loop:
    lda BlockGrid, x
    beq :+
    sta (AddressPointer1), y
:
    inx
    iny
    cpy #4
    bcc @loop

    ldy #0
    cpx #16
    beq @done

    clc
    lda AddressPointer1+0
    adc #BoardWidth
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
    jmp @loop
@done:
    rts

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

DrawFullBoard:
    ldx #0
    ldy #0
    lda #BoardWidth
    sta TmpA
    lda #BoardHeight
    sta TmpB
@loopRow:
    lda PlayfieldPpuRows_Hi, x
    sta $2006
    lda PlayfieldPpuRows_Lo, x
    sta $2006
    inx

    lda #BoardWidth
    sta TmpA

@loopCol:
    lda FieldGrid, y
    sta $2007
    iny
    dec TmpA
    bne @loopCol

    dec TmpB
    bne @loopRow

    rts

DrawFullBoard_SPEED:
    lda #$02
    sta $4014

    .repeat BoardHeight, i
        lda PlayfieldPpuRows_Hi+i
        sta $2006
        lda PlayfieldPpuRows_Lo+i
        sta $2006

        .repeat BoardWidth, j
            lda FieldGrid+(i*BoardWidth)+j
            sta $2007
        .endrepeat
    .endrepeat
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

BlockToPlayfield_Hi:
    .repeat BoardHeight, i
        .byte .hibyte(FieldGrid+(i*BoardWidth))
    .endrepeat

BlockToPlayfield_Lo:
    .repeat BoardHeight, i
        .byte .lobyte(FieldGrid+(i*BoardWidth))
    .endrepeat

PlayfieldStartAddr = $20CC
PlayfieldPpuRows_Hi:
    .repeat BoardHeight, i
        .byte .hibyte(PlayfieldStartAddr+(i*32))
    .endrepeat

PlayfieldPpuRows_Lo:
    .repeat BoardHeight, i
        .byte .lobyte(PlayfieldStartAddr+(i*32))
    .endrepeat

BlockStart_Y:
    .byte 1 ; Z
    .byte 1 ; S
    .byte 1 ; T
    .byte 1 ; L
    .byte 1 ; J
    .byte 0 ; I
    .byte 0 ; O

; Locations for each tile in a block
BlockTiles:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word :+++++
    .word :++++++
    .word :+++++++

; Z
; XX
;  XX
:   .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; S
;  XX
; XX
:   .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_A, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_A, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; T
;  X
; XXX
:   .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X


; L
; XXX
; X
:   .byte TILE_X, TILE_X, TILE_A, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_X
    .byte TILE_A, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; J
; X
; XXX
:   .byte TILE_A, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; I
; XXXX
:   .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_A
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_A, TILE_A, TILE_A, TILE_A
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_X, TILE_X

; O
; XX
; XX
:   .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_A, TILE_A, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

