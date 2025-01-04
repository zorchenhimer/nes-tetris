.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 1
BlockRotation: .res 1
DropSpeed: .res 1

SoftDrop: .res 1

Level_Tiles: .res 4
Score_Tiles: .res 6
Lines_Tiles: .res 6
Combo_Tiles: .res 4

TmpM: .res 3

MathA: .res 2
MathB: .res 2
MathC: .res 2

BottomVals: .res 4
TopVals: .res 4
LowestRows: .res 4

TmpBlockOffset: .res 1

.segment "BSS"
BlockGrid: .res 4*4
BlockX: .res 1
BlockY: .res 1

CurrentX: .res 1
CurrentY: .res 1
GhostY: .res 1

MinX: .res 1
MaxX: .res 1

; Which rows need to be cleared
ClearRows: .res 20

BagLeft: .res 1
BagA: .res 7 ; next pieces
BagB: .res 7 ; next pieces
HoldPiece: .res 1

FieldGrid: .res 10*20

HeldSwapped: .res 1

Level: .res 2
Score: .res 3
Lines: .res 3
Combo: .res 2

TmpScore: .res 3

; Contains tile indicies
HighScore: .res 6

ClearCount: .res 1 ; rows cleared this frame
DropScore: .res 1  ; soft and hard drop scores this frame

LowestY: .res 1
GhostYBase: .res 1
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

GAMEOVER_START_X = 104
GAMEOVER_START_Y = 109

;DEBUG_PIECE = 6
;DEBUG_FIELD = 1

BlockGridOffset_X = -2
BlockGridOffset_Y = -1

SCORE_ADDR = $2202
LINES_ADDR = $2262
COMBO_ADDR = $21A4
LEVEL_ADDR = $22C4

.enum IRQStates
DrawBoard
.endenum

IrqStates:
    .word irqDrawBoard

IrqLines:
    .byte 10

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

DisableIrq:
    lda #$00
    sta $5204
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

    lda #.hibyte(SCORE_ADDR)
    sta $2006
    lda #.lobyte(SCORE_ADDR)
    sta $2006

    .repeat .sizeof(Score_Tiles), j
    lda Score_Tiles+j
    sta $2007
    .endrepeat

    lda #.hibyte(LINES_ADDR)
    sta $2006
    lda #.lobyte(LINES_ADDR)
    sta $2006

    .repeat .sizeof(Lines_Tiles), j
    lda Lines_Tiles+j
    sta $2007
    .endrepeat

    lda #.hibyte(COMBO_ADDR)
    sta $2006
    lda #.lobyte(COMBO_ADDR)
    sta $2006

    .repeat .sizeof(Combo_Tiles), j
    lda Combo_Tiles+j
    sta $2007
    .endrepeat

    lda #.hibyte(LEVEL_ADDR)
    sta $2006
    lda #.lobyte(LEVEL_ADDR)
    sta $2006

    .repeat .sizeof(Level_Tiles), j
    lda Level_Tiles+j
    sta $2007
    .endrepeat

    rts

GamePalettes:
    .byte $0F, $00, $1A, $20
    .byte $0F, $23, $21, $20
    .byte $0F, $2C, $28, $20
    .byte $0F, $15, $27, $20

SpritePalettes:
    .byte $0F, $00, $0A, $20
    .byte $0F, $13, $11, $20
    .byte $0F, $1C, $18, $20
    .byte $0F, $05, $17, $20

GameOverPalette:
    .byte $0F, $15, $27, $20

InitGame:
    ; Clear sprites
    jsr ClearSprites

    lda #.lobyte(GamePalettes)
    sta AddressPointer1+0
    lda #.hibyte(GamePalettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes

    lda #.lobyte(GameOverPalette)
    sta AddressPointer1+0
    lda #.hibyte(GameOverPalette)
    sta AddressPointer1+1
    ldx #7
    jsr LoadPalette

    ldx #0
    jsr FillAttributeTable

    lda #.lobyte(Screen_Playfield)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Playfield)
    sta AddressPointer1+1
    jsr DrawScreen

    lda #0
    ldy #0
:
    .ifdef DEBUG_FIELD
    lda DebugField, y
    .endif
    sta FieldGrid, y
    iny
    cpy #200
    bne :-

    jsr InitBags
    jsr NextBlock

    lda #0
    ldx #0
:
    sta Level, x
    inx
    cpx #.sizeof(Level) + .sizeof(Score) + .sizeof(Lines) + .sizeof(Combo)
    bne :-

    lda #'0'
    ldx #0
:
    sta HighScore, x
    inx
    cpx #.sizeof(HighScore)
    bne :-

    lda #1
    sta Level

    ; Turn off ExtAttr mode to draw the initial attribute data
    lda #%0000_0010
    sta $5104

    lda #$C0
    ldx #0
:
    sta $5C00+(0*256), x
    sta $5C00+(1*256), x
    sta $5C00+(2*256), x
    sta $5C00+(3*256), x
    dex
    bne :-

    ; Turn ExtAttr mode back on
    lda #%0000_0001
    sta $5104

    lda #%1000_0000
    sta $2000

    lda #$00
    sta $2003

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

    lda #$00
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    jsr WaitForNMI
    jsr WaitForNMI

    lda #.lobyte(NmiGame)
    sta NmiHandler+0
    lda #.hibyte(NmiGame)
    sta NmiHandler+1

    lda #SPEED
    sta DropSpeed

    lda #$FF
    sta HoldPiece

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

    lda #BUTTON_SELECT ; select
    jsr ButtonReleased
    beq :+
    jsr SwapHeldPiece
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
    lda SoftDrop
    beq :+
    lda #1
    sta DropScore
:
    inc BlockY
    lda #SPEED
    sta DropSpeed
    jsr CheckFallCollision
@noDrop:

    ; TODO: score stuff
    jsr CalcScore

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

ClearCountScores:
    .byte 0, 1, 3, 5, 8

CalcScore:
    clc
    lda DropScore
    adc Score+0
    sta Score+0

    lda Score+1
    adc #0
    sta Score+1

    lda Score+2
    adc #0
    sta Score+2

    lda #0
    sta TmpM+0
    sta TmpM+1
    sta TmpM+2

    ; Level * Clear Score
    ldx ClearCount
    bne :+
    jmp @done
:
    lda ClearCountScores, x
    sta MMC5_MultA
    lda Level
    sta MMC5_MultB

    lda MMC5_MultA
    sta MathA+0
    lda MMC5_MultB
    sta MathA+1

    ; Clear Score lo * 100
    lda MathA+0
    sta MMC5_MultA
    lda #100
    sta MMC5_MultB

    lda MMC5_MultA
    sta MathB+0
    lda MMC5_MultB
    sta MathB+1

    ; Clear Score hi * 100
    lda MathA+1
    sta MMC5_MultA
    lda #100
    sta MMC5_MultB

    lda MMC5_MultA
    sta MathC+0
    lda MMC5_MultB
    sta MathC+1

    lda MathB+0
    sta TmpM+0

    lda MathB+1
    sta TmpM+1

    clc
    lda MathC+0
    adc TmpM+1
    sta TmpM+1

    lda MathC+1
    adc TmpM+2
    sta TmpM+2

    clc
    lda Score+0
    adc TmpM+0
    sta Score+0

    lda Score+1
    adc TmpM+1
    sta Score+1

    lda Score+2
    adc TmpM+2
    sta Score+2

@done:

    clc
    lda Lines
    adc ClearCount
    sta Lines

    .repeat .sizeof(Score), i
    lda Score+i
    ;sta TmpScore+i
    sta Bin_Input+i
    .endrepeat

    jsr BinToDec

    .repeat .sizeof(Bin_Tiles), i
    lda Bin_Tiles+i
    sta Score_Tiles+i
    .endrepeat

    .repeat .sizeof(Lines), i
    lda Lines+i
    sta Bin_Input+i
    .endrepeat

    jsr BinToDec

    .repeat .sizeof(Bin_Tiles), i
    lda Bin_Tiles+i
    sta Lines_Tiles+i
    .endrepeat

    lda #0
    sta ClearCount
    sta DropScore
    rts

DedTransition:
    jsr UpdateBlock
    jsr WaitForNMI
    jsr DisableIrq

    ; game
    ; over

    ;ldx #0
    ;ldy #$A0
    .repeat 8, i
        lda #GAMEOVER_START_Y
        sta GameOverSprites+(i*4)+0

        lda #$A0+i
        sta GameOverSprites+(i*4)+1

        lda #$03
        sta GameOverSprites+(i*4)+2

        lda #GAMEOVER_START_X+(i*8)
        sta GameOverSprites+(i*4)+3
    .endrepeat
    ; ----
    .repeat 8, i
        lda #GAMEOVER_START_Y+8
        sta GameOverSprites+(i*4)+0+(1*8*4)

        lda #$B0+i
        sta GameOverSprites+(i*4)+1+(1*8*4)

        lda #$03
        sta GameOverSprites+(i*4)+2+(1*8*4)

        lda #GAMEOVER_START_X+(i*8)
        sta GameOverSprites+(i*4)+3+(1*8*4)
    .endrepeat

    ; ----
    .repeat 8, i
        lda #GAMEOVER_START_Y+16+4
        sta GameOverSprites+(i*4)+0+(2*8*4)

        lda #$C0+i
        sta GameOverSprites+(i*4)+1+(2*8*4)

        lda #$03
        sta GameOverSprites+(i*4)+2+(2*8*4)

        lda #GAMEOVER_START_X+(i*8)
        sta GameOverSprites+(i*4)+3+(2*8*4)
    .endrepeat
    ; ----
    .repeat 8, i
        lda #GAMEOVER_START_Y+24+4
        sta GameOverSprites+(i*4)+0+(3*8*4)

        lda #$D0+i
        sta GameOverSprites+(i*4)+1+(3*8*4)

        lda #$03
        sta GameOverSprites+(i*4)+2+(3*8*4)

        lda #GAMEOVER_START_X+(i*8)
        sta GameOverSprites+(i*4)+3+(3*8*4)
    .endrepeat

    .repeat 8, i
        lda #125
        sta GameOverOops+(i*4)

        lda #$90
        sta GameOverOops+(i*4)+1

        lda #$03
        sta GameOverOops+(i*4)+2

        lda #GAMEOVER_START_X+(i*8)
        sta GameOverOops+(i*4)+3
    .endrepeat
    jsr WaitForNMI

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
    sta ClearCount ; total number of cleared rows
:
    sta ClearRows, x
    dex
    bpl :-

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
    inc ClearCount
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
    ; TODO: Precalculate this shit.  Might want to precalc
    ;       the rotations.  Eg, layout in Tiled then calc
    ;       grids and all lookup tables with a util.
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
    ; TODO: Use an offset instead of pointer
    ;       address.  See CalculateGhost.
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

SwapHeldPiece:
    lda HeldSwapped
    beq :+
    rts
:
    lda #$FF
    sta HeldSwapped

    lda HoldPiece
    bpl @swap
    ; nothing was held
    lda CurrentBlock
    sta HoldPiece
    jmp NextBlock_Swap
@swap:

    lda HoldPiece
    pha
    lda CurrentBlock
    sta HoldPiece
    pla
    sta CurrentBlock

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

NextBlock:
    lda #$00
    sta HeldSwapped

NextBlock_Swap:
    lda #SPEED
    sta DropSpeed

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

    .ifdef DEBUG_PIECE
    lda #DEBUG_PIECE
    sta CurrentBlock
    .endif

    lda #BLOCK_START_X
    sta BlockX

    ldy CurrentBlock
    lda BlockStart_Y, y
    sta BlockY

    lda #0
    sta BlockRotation

    lda #0
    sta SoftDrop

    jsr LoadBlock
    jsr CheckCollide_WithGrid
    beq :+
    jmp DedTransition
:
    rts

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

    lda #0
    sta HeldSwapped
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

CalculateGhost:

    ;
    ; Copy top & bottom vals to zero page
    lda CurrentBlock
    asl a
    asl a
    asl a
    asl a
    sta TmpBlockOffset

    lda BlockRotation
    asl a
    asl a
    clc
    adc TmpBlockOffset
    tay

    ldx #0
:
    lda BlockBottoms, y
    sta BottomVals, x

    lda BlockTops, y
    sta TopVals, x

    lda #21
    sta LowestRows, x
    inx
    iny
    cpx #4
    bne :-

    lda #$FF
    sta LowestY

    lda #0
    sta TmpA ; Current column

    ; Find the highest row in each column.
    ; This is limited to the four columns that
    ; the piece grid occupies.
@fieldColTopLoop:
    ldx TmpA
    lda TopVals, x
    clc
    adc CurrentY
    sta TmpB ; current row
    dec TmpB

    ; Align to playfield, but we want the offset
    ; (y-1)*10+x-1 = y*10+x-1-10 = y*10+x-11
    ;lda CurrentY
    sta MMC5_MultA
    lda #10
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc CurrentX
    adc TmpA
    sec
    sbc #11

    ; Offset in the field for the first
    ; column in the BlockGrid
    sta TmpY

    ldy TmpY
@fieldColLoop:
    lda FieldGrid, y
    beq @nextFieldRow

    lda TmpB
    tay
    iny
    tya
    ldy TmpA
    sta LowestRows, y
    jmp @fieldNextColumn

@nextFieldRow:
    inc TmpB
    lda TmpB
    cmp #$14
    beq @fieldNextColumn
    tya
    clc
    adc #10
    tay
    jmp @fieldColLoop

@fieldNextColumn:
    inc TmpY ; offset
    ldy TmpY
    inc TmpA ; col
    lda TmpA
    cmp #4
    bne @fieldColTopLoop

    ; Find the highest coordinate across
    ; all four columns of the piece grid
    ldx #0
@bottomLoop:
    lda BottomVals, x
    beq @nextBottom

    sec
    lda LowestRows, x
    sbc BottomVals, x
    cmp LowestY
    bcs @nextBottom
    sta LowestY
    ;inc LowestY

@nextBottom:
    inx
    cpx #4
    bne @bottomLoop

    lda LowestY
    sta GhostY
    rts

CalculateGhost_borked:
;
; Figure out ghost position
    lda #$FF
    sta LowestY
    lda CurrentBlock
    asl a
    tax

    lda BlockRotation
    asl a
    asl a
    sta TmpX

    clc
    lda BlockBottoms+0, x
    adc TmpX
    sta AddressPointer1+0
    lda BlockBottoms+1, x
    adc #0
    sta AddressPointer1+1

    ; Align to playfield, but we want the offset
    ; (y-1)*10+x-1 = y*10+x-1-10 = y*10+x-11
    lda CurrentY
    sta MMC5_MultA
    lda #10
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc CurrentX
    sec
    sbc #11
    sta TmpY ; Offset in the field for the first
             ; column in the BlockGrid

    lda CurrentY
    sta TmpA

    lda #0
    sta TmpB ; column
    ; Check block column lowest row
@loopTop:
    lda TmpA
    sta TmpC ; current row
    ldy TmpB
    lda (AddressPointer1), y
    beq @nextBlockColumn

    ldx TmpY
@rowLoop:
    lda FieldGrid, x
    beq @nextFieldRow

    ldy TmpB
    sec
    lda TmpC
    sbc (AddressPointer1), y
    cmp LowestY
    bcs @nextBlockColumn
    sta LowestY
    jmp @nextBlockColumn

@nextFieldRow:
    inc TmpC
    txa
    clc
    adc #10
    cmp #200
    bcs @nextBlockColumn
    jmp @rowLoop

@nextBlockColumn:
    ldy TmpB
    iny
    cpy #4
    beq @done
    sty TmpB
    inc TmpY
    jmp @loopTop

@done:

    lda LowestY
    bpl @notBottom ; check for bottom of board
    ldy #0
    lda (AddressPointer1), y
    sta TmpX
    iny
    lda (AddressPointer1), y
    cmp TmpX
    bcs :+
    sta TmpX
:   lda (AddressPointer1), y
    cmp TmpX
    bcs :+
    sta TmpX
:   lda (AddressPointer1), y
    cmp TmpX
    bcs :+
    sta TmpX
@notBottom:
    sta GhostY
    rts

UpdateBlock:
    jsr CalculateGhost

;
; Update both block and ghost sprites
    ldx CurrentX
    lda BlockGridLocationX, x
    sta TmpX

    ldx CurrentY
    lda BlockGridLocationY, x
    sta TmpY

    ldx GhostY
    lda BlockGridLocationY, x
    sta TmpZ

    ldx #0
    ldy #0
@loop:
    lda BlockGrid, x
    sta TmpA
    beq @next
    sta TmpB
    clc
    lda BlockSpriteLookupY, x
    adc TmpY
    sta SpriteBlock, y

    lda FrameCount
    and #$01
    beq :+
    clc
    lda BlockSpriteLookupY, x
    adc TmpZ
    jmp :++
:   lda #$FF
:   sta GhostBlock, y
    iny

    lda TmpA
    and #$03
    ora #$10
    sta SpriteBlock, y
    sta GhostBlock, y
    iny

    lda #0
    sta SpriteBlock, y
    lda #1
    sta GhostBlock, y
    iny

    clc
    lda BlockSpriteLookupX, x
    adc TmpX
    sta SpriteBlock, y
    sta GhostBlock, y
    iny

@next:
    inx
    cpx #16
    bne @loop

    lda TmpB
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    ldy #0
:
    ; Active block
    lda GamePalettes, x
    sta Palettes+16, y

    ; Ghost block
    lda SpritePalettes, x
    sta Palettes+20, y
    inx
    iny
    cpy #4
    bne :-


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


HOLD_ADDR  = $20A3 + MMC5_OFFSET
irqDrawBoard:

    bit HoldPiece
    bpl :+
    jmp @noHold
:
    ;
    ; Draw hold piece
    lda HoldPiece
    asl a
    tax
    lda BlockTiles+0, x
    sta AddressPointer1+0
    lda BlockTiles+1, x
    sta AddressPointer1+1

    lda HoldPiece
    cmp #5
    bcc :+
    clc
    lda AddressPointer1+0
    adc #4
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
:

    ldy #0
    .repeat 2, i
        .repeat 4, j
            lda (AddressPointer1), y
            sta HOLD_ADDR + (i*32) + j
            iny
        .endrepeat
    .endrepeat
@noHold:

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
        cmp #5
        bcc :+
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
    .repeat 10
    lda (AddressPointer2), y
    sta (AddressPointer1), y
    iny
    .endrepeat

    inx
    cpx #20
    bne @loopRow

    ; TODO: IRQSleep, similar to the NMI sleep

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

BlockLeft:
    .byte 0, 1, 0, 0 ; Z
    .byte 0, 1, 0, 0 ; S
    .byte 0, 1, 0, 0 ; T
    .byte 0, 1, 0, 0 ; L
    .byte 0, 1, 0, 0 ; J
    .byte 0, 2, 0, 1 ; I
    .byte 1, 1, 1, 1 ; O

BlockRight:
    .byte 1, 1, 1, 2 ; Z
    .byte 1, 1, 1, 2 ; S
    .byte 1, 1, 1, 2 ; T
    .byte 1, 1, 1, 2 ; L
    .byte 1, 1, 1, 2 ; J
    .byte 0, 1, 0, 2 ; I
    .byte 1, 1, 1, 1 ; O

BlockTops:
    ; Z
    .byte 0, 0, 1, 4
    .byte 4, 1, 0, 4
    .byte 1, 1, 2, 4
    .byte 1, 0, 4, 4

    ; S
    .byte 1, 0, 0, 4
    .byte 4, 0, 1, 4
    .byte 2, 1, 1, 4
    .byte 0, 1, 4, 4

    ; T
    .byte 1, 0, 1, 4
    .byte 4, 0, 1, 4
    .byte 1, 1, 1, 4
    .byte 1, 0, 4, 4

    ; L
    .byte 1, 1, 0, 4
    .byte 4, 0, 2, 4
    .byte 1, 1, 1, 4
    .byte 0, 0, 4, 4

    ; J
    .byte 0, 1, 1, 4
    .byte 4, 0, 0, 4
    .byte 1, 1, 1, 4
    .byte 2, 0, 4, 4

    ; I
    .byte 1, 1, 1, 1
    .byte 4, 4, 0, 4
    .byte 2, 2, 2, 2
    .byte 4, 0, 4, 4

    ; O
    .byte 4, 1, 1, 4
    .byte 4, 1, 1, 4
    .byte 4, 1, 1, 4
    .byte 4, 1, 1, 4

BlockBottoms:
    ; Z
    .byte 1, 2, 2, 0
    .byte 0, 3, 2, 0
    .byte 2, 3, 3, 0
    .byte 3, 2, 0, 0

    ; S
    .byte 2, 2, 1, 0
    .byte 0, 2, 3, 0
    .byte 3, 3, 2, 0
    .byte 2, 3, 0, 0

    ; T
    .byte 2, 2, 2, 0
    .byte 0, 3, 2, 0
    .byte 2, 3, 2, 0
    .byte 2, 3, 0, 0

    ; L
    .byte 2, 2, 2, 0
    .byte 0, 3, 3, 0
    .byte 3, 2, 2, 0
    .byte 1, 3, 0, 0

    ; J
    .byte 2, 2, 2, 0
    .byte 0, 3, 1, 0
    .byte 2, 2, 3, 0
    .byte 3, 3, 0, 0

    ; I
    .byte 2, 2, 2, 2
    .byte 0, 0, 4, 0
    .byte 3, 3, 3, 3
    .byte 0, 4, 0, 0

    ; O
    .byte 0, 3, 3, 0
    .byte 0, 3, 3, 0
    .byte 0, 3, 3, 0
    .byte 0, 3, 3, 0

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
TILE_Z = TILE_1 | PAL_D
:   .byte TILE_Z, TILE_Z, TILE_X, TILE_X
    .byte TILE_X, TILE_Z, TILE_Z, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_Z, TILE_X
    .byte TILE_X, TILE_Z, TILE_Z, TILE_X
    .byte TILE_X, TILE_Z, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_Z, TILE_Z, TILE_X, TILE_X
    .byte TILE_X, TILE_Z, TILE_Z, TILE_X
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

    .byte TILE_X, TILE_S, TILE_X, TILE_X
    .byte TILE_X, TILE_S, TILE_S, TILE_X
    .byte TILE_X, TILE_X, TILE_S, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_S, TILE_S, TILE_X
    .byte TILE_S, TILE_S, TILE_X, TILE_X
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

    .byte TILE_X, TILE_X, TILE_I, TILE_X
    .byte TILE_X, TILE_X, TILE_I, TILE_X
    .byte TILE_X, TILE_X, TILE_I, TILE_X
    .byte TILE_X, TILE_X, TILE_I, TILE_X

    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_X, TILE_X, TILE_X, TILE_X
    .byte TILE_I, TILE_I, TILE_I, TILE_I
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
