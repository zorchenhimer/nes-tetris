.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 1
BlockRotation: .res 1
DropSpeed: .res 1

SoftDrop: .res 1

.segment "BSS"
BlockGrid: .res 4*4
BlockX: .res 1
BlockY: .res 1

CurrentX: .res 1
CurrentY: .res 1

MinX: .res 1
MaxX: .res 1

; Which rows need to be cleared
ClearRows: .res 20

BagLeft: .res 1
BagA: .res 7 ; next pieces
BagB: .res 7 ; next pieces
HoldPiece: .res 1

FieldGrid: .res 10*20

.popseg

SPEED = 60
SOFT_SPEED = 35

BlockLocation_X = 88
BlockLocation_Y = 40 - 1
Block_TileId = $10

BoardHeight = 20
BoardWidth  = 10

; These are CHR banks, not tiles
TILE_X = $00
TILE_1 = $01
TILE_2 = $02

PAL_A  = $00
PAL_B  = $40
PAL_C  = $80
PAL_D  = $C0

; Column offset for bounding box
BLOCK_START_X = 4

MMC5_OFFSET = $3C00

.enum IRQStates
DrawBoard
.endenum

IrqStates:
    .word irqDrawBoard
    ;.word irqDrawBags

IrqLines:
    .byte 10
    .byte 130

; IRQ state index in A
SetIRQ:
    ;lda NextIRQ
    tax
    asl a
    tay

    lda IrqLines, x
    sta $5203
    lda #$80
    sta $5204

    lda IrqStates+0, y
    sta ptrIRQ+0
    lda IrqStates+1, y
    sta ptrIRQ+1
    cli
    rts

NmiGame:
    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    .repeat 4*8, i
        lda Palettes+i
        sta $2007
    .endrepeat

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    rts

GamePalettes:
    .byte $0F, $15, $1A, $20
    .byte $0F, $23, $21, $20
    .byte $0F, $2C, $28, $20
    .byte $0F, $10, $27, $20

InitGame:
    ; Clear sprites
    jsr ClearSprites

    .repeat 4*4, i
        lda GamePalettes+i
        sta Palettes+i
    .endrepeat

    .repeat 4*4, i
        lda GamePalettes+i
        sta Palettes+i+16
    .endrepeat

    ldx #0
    jsr FillAttributeTable

    lda #.lobyte(Screen_Playfield)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Playfield)
    sta AddressPointer1+1
    jsr DrawScreen

    jsr InitBags

    jsr NextBlock

    lda #%1000_0000
    sta $2000

    lda #$00
    sta $2003
    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    lda #.lobyte(NmiGame)
    sta NmiHandler+0
    lda #.hibyte(NmiGame)
    sta NmiHandler+1

    lda #SPEED
    sta DropSpeed

FrameGame:
    lda #IRQStates::DrawBoard
    jsr SetIRQ

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

    lda #BUTTON_DOWN ; down, pressed
    jsr ButtonPressed
    beq :+
    lda #1
    sta SoftDrop
:

    lda #BUTTON_DOWN ; down, released
    jsr ButtonReleased
    beq :+
    lda #0
    sta SoftDrop
:

    lda #BUTTON_A ; A
    jsr ButtonPressed
    beq :+
    inc BlockRotation
    lda #$03
    and BlockRotation
    sta BlockRotation

    jsr LoadBlock
    jsr CheckCollide_Rotate
    beq :+
    ; Rotation collides; rotate back
    dec BlockRotation
    lda #$03
    and BlockRotation
    sta BlockRotation
    jsr LoadBlock
:

    lda #BUTTON_B ; B
    jsr ButtonPressed
    beq :+
    dec BlockRotation
    lda #$03
    and BlockRotation
    sta BlockRotation

    jsr LoadBlock
    jsr CheckCollide_Rotate
    beq :+
    ; Rotation collides; rotate back
    inc BlockRotation
    lda #$03
    and BlockRotation
    sta BlockRotation
    jsr LoadBlock
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :++
    dec BlockX
    bpl :+
    lda #0
    sta BlockX
:
    jsr CheckCollide_Rotate
    beq :+
    inc BlockX
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :++
    inc BlockX
    lda BlockX
    cmp #BoardWidth
    bcc :+
    lda #BoardWidth-1
    sta BlockX
:
    jsr CheckCollide_Rotate
    beq :+
    dec BlockX
:

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
:

    lda SoftDrop
    beq @noSoft
    sec
    lda DropSpeed
    sbc #SOFT_SPEED
    sta DropSpeed
    bpl @noDrop
    jmp @doDrop

@noSoft:

    dec DropSpeed
    bpl @noDrop
@doDrop:
    inc BlockY
    lda #SPEED
    sta DropSpeed
    jsr CheckFallCollision
@noDrop:

    lda BlockX
    sta CurrentX
    lda BlockY
    sta CurrentY

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

InitBags:
    lda #7
    sta BagLeft
    lda #$FF
    ldx #0
:
    sta BagA, x
    sta BagB, x
    inx
    cpx #7
    bne :-

    lda #0
    sta TmpX

@loopA:
    inc rng_index
    ldx rng_index
    lda PieceRng, x

    ldy #0
@checkA:
    cmp BagA, y
    beq @loopA
    iny
    cpy #7
    bne @checkA

    ldx TmpX
    sta BagA, x
    inc TmpX
    cpx #6
    bne @loopA

    lda #0
    sta TmpX

@loopB:
    inc rng_index
    ldx rng_index
    lda PieceRng, x

    ldy #0
@checkB:
    cmp BagB, y
    beq @loopB
    iny
    cpy #7
    bne @checkB

    ldx TmpX
    sta BagB, x
    inc TmpX
    cpx #6
    bne @loopB

    rts

ShuffleBag:
    lda #7
    sta BagLeft
    lda #$FF
    ldx #0
:
    sta BagB, x
    inx
    cpx #7
    bne :-

    lda #0
    sta TmpX

@loop:
    inc rng_index
    ldx rng_index
    lda PieceRng, x

    ldy #0
@check:
    cmp BagB, y
    beq @loop
    iny
    cpy #7
    bne @check

    ldx TmpX
    sta BagB, x
    inc TmpX
    cpx #6
    bne @loop

    rts


; Doesn't do any rotational collision stuff, just block gravity.
; Handles placing a block on the playfield
CheckFallCollision:
    ; Find lowest row
    ldx #15
@bottomLoop:
    lda BlockGrid, x
    bne @found
    dex
    jmp @bottomLoop
@found:
    txa
    lsr a
    lsr a
    clc
    adc BlockY
    sta TmpA
    cmp #BoardHeight+1
    bcc :+
    jmp @CollideBottom
:

    jsr CheckCollide_WithGrid
    bne @CollideBottom
    rts

@CollideBottom:
    dec BlockY
    jsr PlaceBlock

    inc CurrentBlock
    lda CurrentBlock
    cmp #7
    bne :+
    lda #0
    sta CurrentBlock
:
    jsr CheckRowClear
    ldx #.sizeof(ClearRows)-1
:   lda ClearRows, x
    bne StartClearRows
    dex
    bpl :-

@no:
    jsr NextBlock
    rts

StartClearRows:
    stx TmpA ; First row index to clear
    stx TmpB ; Source row to copy from

@top:
    ; Destination
    ldx TmpA
    bmi @bottom
    lda BlockToPlayfield_Lo, x
    sta AddressPointer1+0
    lda BlockToPlayfield_Hi, x
    sta AddressPointer1+1

:
    dec TmpB
    bpl :+
    jmp @ClearWithEmpty
:

    ; Do we need to clear multiple rows?
    ldx TmpB
    lda ClearRows, x
    bne :--

    ; Source
    ldx TmpB
    lda BlockToPlayfield_Lo, x
    sta AddressPointer2+0
    lda BlockToPlayfield_Hi, x
    sta AddressPointer2+1

    jsr DoClearRow

@bottom:
    dec TmpA
    bpl @top
    jmp NextBlock
    ;rts

; Clear the top row(s)
@ClearWithEmpty:
    ldy #0
    lda #0
:   sta (AddressPointer1), y
    iny
    cpy #10
    bne :-
    jmp @bottom


CheckRowClear:
    ldx #.sizeof(ClearRows)-1
    lda #0
:
    sta ClearRows, x
    dex
    bpl :-


;    ldx #15
;@bottomLoop:
;    lda BlockGrid, x
;    bne @found
;    dex
;    jmp @bottomLoop
;@found:
;    txa
;    lsr a
;    lsr a
;    clc
;    adc BlockY
;    sta BlockY

    inc BlockY
    inc BlockY
    inc BlockY

    lda #4
    sta TmpX
@loop:
    dec BlockY
    bmi @next ; too high, don't check against nothing
    ldy BlockY
    cpy #BoardHeight
    bcs @next ; too low, don't check against nothing
    lda BlockToPlayfield_Lo, y
    sta AddressPointer1+0

    lda BlockToPlayfield_Hi, y
    sta AddressPointer1+1

    jsr CheckSingleRow
    beq :+
    ldy BlockY
    lda #1
    sta ClearRows, y
:

@next:
    dec TmpX
    bne @loop
    rts

; Row in AddressPointer1
CheckSingleRow:
    ldy #0
    .repeat 10
        lda (AddressPointer1), y
        beq @nope
        iny
    .endrepeat

    lda #1
@nope:
    rts

DoClearRow:
    ldy #0
:
    lda (AddressPointer2), y
    sta (AddressPointer1), y
    iny
    cpy #10
    bne :-
    rts

CheckCollide_Rotate:
    lda #BoardWidth
    sta MaxX
    lda #0 ; no block in col 1
    sta MinX

    ; Left - Col 1
    .repeat 4, i
        lda BlockGrid+(i*4)
        bne @FailCol1
    .endrepeat
    jmp @checkCol4

@FailCol1: ; block in col 1
    lda #1
    sta MinX

@checkCol4:
    ; Col 4
    .repeat 4, i
        lda BlockGrid+(i*4)+3
        bne @FailCol4
    .endrepeat
    jmp @checkCol3

@FailCol4:
    lda #BoardWidth-2
    sta MaxX
    jmp @checkLeft

@checkCol3:
    ; Col 3
    .repeat 4, i
        lda BlockGrid+(i*4)+2
        bne @FailCol3
    .endrepeat
    jmp @checkLeft

@FailCol3:
    lda #BoardWidth-1
    sta MaxX

@checkLeft:
    lda BlockX
    cmp MinX
    bcs @checkRight
    lda #1  ; can't rotate; on left edge
    rts

@checkRight:
    lda BlockX
    cmp MaxX
    bcc :+
    lda #1  ; can't rotate; on right edge
    rts

:

CheckCollide_WithGrid:
    ; Align grid on playfield
    ldy BlockY
    dey
    bpl :+
    ldy #0
    lda BlockToPlayfield_Lo, y
    clc
    adc BlockX
    sta AddressPointer1+0

    lda BlockToPlayfield_Hi, y
    adc #0
    sta AddressPointer1+1

    lda AddressPointer1+0
    sec
    sbc #1
    sta AddressPointer1+0

    lda AddressPointer1+1
    sbc #0
    sta AddressPointer1+1

    ldy #3
    ldx #3
    jmp @nextBlock
:
    lda BlockToPlayfield_Lo, y
    clc
    adc BlockX
    sta AddressPointer1+0

    lda BlockToPlayfield_Hi, y
    adc #0
    sta AddressPointer1+1

    lda AddressPointer1+0
    sec
    sbc #1
    sta AddressPointer1+0

    lda AddressPointer1+1
    sbc #0
    sta AddressPointer1+1

    ldx #0
    ldy #0
@blockLoop:
    lda BlockGrid, x
    beq @nextBlock
    lda (AddressPointer1), y
    beq @nextBlock
    jmp @collide

@nextBlock:
    iny
    cpy #4
    bne :+
    ldy #0
    clc
    lda AddressPointer1+0
    adc #BoardWidth
    sta AddressPointer1+0

    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
:

    inx
    cpx #16
    beq @done
    jmp @blockLoop

@done:
    lda #0
    rts

@collide:
    lda #1
    rts

NextBlock:
    dec BagLeft
    bne :+
    ; new bag
    jsr ShuffleBag
:
    lda BagA+0
    sta CurrentBlock

    ; bump both bags
    ldx #0
:
    lda BagA+1, x
    sta BagA, x
    inx
    cpx #13
    bne :-

    lda #BLOCK_START_X
    sta BlockX

    ldy CurrentBlock
    lda BlockStart_Y, y
    sta BlockY

    lda #0
    sta BlockRotation

    lda #0
    sta SoftDrop

    jmp LoadBlock
    ;rts

PlaceBlock:
    ldy BlockY
    dey
    lda BlockToPlayfield_Lo, y
    clc
    adc BlockX
    sta AddressPointer1+0

    lda BlockToPlayfield_Hi, y
    adc #0
    sta AddressPointer1+1

    lda AddressPointer1+0
    sec
    sbc #1
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
    ldx CurrentX
    lda BlockGridLocationX, x
    sta TmpX

    ldx CurrentY
    lda BlockGridLocationY, x
    sta TmpY

    ldx #0
    ldy #0
@loop:
    lda BlockGrid, x
    sta TmpA
    beq @next
    clc
    lda BlockSpriteLookupY, x
    adc TmpY
    sta SpriteBlock, y
    iny

    ;lda #Block_TileId
    lda TmpA
    and #$03
    ora #$10
    sta SpriteBlock, y
    iny

    lda TmpA
    lsr a
    lsr a
    lsr a
    lsr a
    lsr a
    lsr a
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
        lda #.hibyte(PlayfieldStartAddr+(i*32))
        ;lda PlayfieldPpuRows_Hi+i
        sta $2006
        ;lda PlayfieldPpuRows_Lo+i
        lda #.lobyte(PlayfieldStartAddr+(i*32))
        sta $2006

        .repeat BoardWidth, j
            lda FieldGrid+(i*BoardWidth)+j
            sta $2007
        .endrepeat
    .endrepeat
    rts

irqDrawBoard:
    ;
    ; Draw bag contents
    .repeat 4, j
        lda BagA+j
        asl a
        tax
        lda BlockTiles+0, x
        sta AddressPointer1+0
        lda BlockTiles+1, x
        sta AddressPointer1+1

        lda BagA+j
        cmp #6
        bne :+
        clc
        lda AddressPointer1+0
        adc #4
        sta AddressPointer1+0
        lda AddressPointer1+1
        adc #0
        sta AddressPointer1+1
:

        .repeat 2, i
            lda BagRows0+0+(i*2)+(j*2*2)
            sta AddressPointer2+0
            lda BagRows0+1+(i*2)+(j*2*2)
            sta AddressPointer2+1

            ldy #0
            .repeat 4
                lda (AddressPointer1), y
                sta (AddressPointer2), y
                iny
            .endrepeat

            clc
            lda AddressPointer1+0
            adc #4
            sta AddressPointer1+0
            lda AddressPointer1+1
            adc #0
            sta AddressPointer1+1
        .endrepeat
    .endrepeat

    ;
    ; Draw playfield
    ldx #0
@loopRow:
    lda PlayfieldPpuRows_Lo, x
    sta AddressPointer1+0
    lda PlayfieldPpuRows_Hi, x
    sta AddressPointer1+1

    lda BlockToPlayfield_Lo, x
    sta AddressPointer2+0
    lda BlockToPlayfield_Hi, x
    sta AddressPointer2+1

    ldy #0
:
    lda (AddressPointer2), y
    sta (AddressPointer1), y
    iny
    cpy #10
    bne :-

    inx
    cpx #20
    bne @loopRow

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

PlayfieldStartAddr = $20CC + MMC5_OFFSET
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
TILE_Z = TILE_1 | PAL_A
:   .byte TILE_Z, TILE_Z, TILE_X, TILE_X
    .byte TILE_X, TILE_Z, TILE_Z, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_Z, TILE_X, TILE_X
    .byte TILE_Z, TILE_Z, TILE_X, TILE_X
    .byte TILE_Z, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_Z, TILE_Z, TILE_X, TILE_X
    .byte TILE_X, TILE_Z, TILE_Z, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_Z, TILE_X, TILE_X
    .byte TILE_Z, TILE_Z, TILE_X, TILE_X
    .byte TILE_Z, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; S
;  XX
; XX
TILE_S = TILE_2 | PAL_A
:   .byte TILE_X, TILE_S, TILE_S, TILE_X
    .byte TILE_S, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_S, TILE_X, TILE_X, TILE_X
    .byte TILE_S, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_S, TILE_S, TILE_X
    .byte TILE_S, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_S, TILE_X, TILE_X, TILE_X
    .byte TILE_S, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; T
;  X
; XXX
TILE_T = TILE_1 | PAL_B
:   .byte TILE_X, TILE_T, TILE_X, TILE_X
    .byte TILE_T, TILE_T, TILE_T, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_T, TILE_X, TILE_X
    .byte TILE_X, TILE_T, TILE_T, TILE_X
    .byte TILE_X, TILE_T, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_T, TILE_T, TILE_T, TILE_X
    .byte TILE_X, TILE_T, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_T, TILE_X, TILE_X
    .byte TILE_T, TILE_T, TILE_X, TILE_X
    .byte TILE_X, TILE_T, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; L
; XXX
; X
TILE_L = TILE_2 | PAL_D
:   .byte TILE_X, TILE_X, TILE_L, TILE_X
    .byte TILE_L, TILE_L, TILE_L, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_L, TILE_X, TILE_X
    .byte TILE_X, TILE_L, TILE_X, TILE_X
    .byte TILE_X, TILE_L, TILE_L, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_L, TILE_L, TILE_L, TILE_X
    .byte TILE_L, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_L, TILE_L, TILE_X, TILE_X
    .byte TILE_X, TILE_L, TILE_X, TILE_X
    .byte TILE_X, TILE_L, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; J
; X
; XXX
TILE_J = TILE_2 | PAL_B
:   .byte TILE_J, TILE_X, TILE_X, TILE_X
    .byte TILE_J, TILE_J, TILE_J, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_J, TILE_J, TILE_X
    .byte TILE_X, TILE_J, TILE_X, TILE_X
    .byte TILE_X, TILE_J, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_J, TILE_J, TILE_J, TILE_X
    .byte TILE_X, TILE_X, TILE_J, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_J, TILE_X, TILE_X
    .byte TILE_X, TILE_J, TILE_X, TILE_X
    .byte TILE_J, TILE_J, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

; I
; XXXX
TILE_I = TILE_1 | PAL_C
:   .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_I, TILE_I, TILE_I, TILE_I
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_I, TILE_X, TILE_X
    .byte TILE_X, TILE_I, TILE_X, TILE_X
    .byte TILE_X, TILE_I, TILE_X, TILE_X
    .byte TILE_X, TILE_I, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_I, TILE_I, TILE_I, TILE_I
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_I, TILE_X, TILE_X
    .byte TILE_X, TILE_I, TILE_X, TILE_X
    .byte TILE_X, TILE_I, TILE_X, TILE_X
    .byte TILE_X, TILE_I, TILE_X, TILE_X

; O
; XX
; XX
TILE_O = TILE_2 | PAL_C
:   .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_O, TILE_O, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

BagRows0:
    .repeat 2, i
        .word $20B9+MMC5_OFFSET+(i*32)+(32*3*0)
    .endrepeat

BagRows1:
    .repeat 2, i
        .word $20B9+MMC5_OFFSET+(i*32)+(32*3*1)
    .endrepeat

BagRows2:
    .repeat 2, i
        .word $20B9+MMC5_OFFSET+(i*32)+(32*3*2)
    .endrepeat

BagRows3:
    .repeat 2, i
        .word $20B9+MMC5_OFFSET+(i*32)+(32*3*3)
    .endrepeat
