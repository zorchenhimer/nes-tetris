.ifndef _GAME_RAM_
_GAME_RAM_ = 1

.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 2
BlockRotation: .res 2
DropSpeed: .res 2

SoftDrop: .res 2
HardDrop: .res 2

Level_Tiles: .res 4
Score_Tiles: .res 8
Lines_Tiles: .res 6
Combo_Tiles: .res 2

Combo_TilesP1: .res 3
Combo_TilesP2: .res 3

Line_TilesP1: .res 4
Line_TilesP2: .res 4

TmpM: .res 3

MathA: .res 3
MathB: .res 2
MathC: .res 2

BottomVals: .res 4
TopVals: .res 4
LowestRows: .res 4

TmpBlockOffset: .res 1

BagTmp: .res 7

GameState:    .res 2
GameStateArg: .res 2
GameStateArgB: .res 2 ; used for clear animation

.segment "BSS"
BlockX: .res 2
BlockY: .res 2

CurrentX: .res 2
CurrentY: .res 2
GhostY: .res 2

MinX: .res 1
MaxX: .res 1

; Which rows need to be cleared
ClearRows: .res 20
ClearRowIdsP1: .res 4 ; Row IDs that need to be cleared
ClearRowIdsP2: .res 4

BagLeft: .res 2 ; 2p
BagP1: .res 14 ; next pieces
BagP2: .res 14 ; next pieces
HoldPiece: .res 2

FieldGrid: .res 10*20
FieldGridP2: .res 10*20

HeldSwapped: .res 2

Level: .res 2

Combo: .res 2 ; one byte per player
Lines: .res 4 ; two bytes per player
UpdateCombo: .res 2

TmpScore: .res 3

; Contains tile indicies
HighScore: .res 6

ClearCount: .res 2 ; rows cleared this frame
DropScore: .res 1  ; soft and hard drop scores this frame

LowestY:    .res 1
GhostYBase: .res 1
DropShake:  .res 1

RepeatLeft: .res 2
RepeatRight: .res 2

LinesToNextLevel: .res 1
NextDropSpeed:    .res 1

Speed_Soft: .res 1
Speed_Drop: .res 1

DirtyLeft: .res 1

Debug_Kick: .res 1

.popseg

SOFT_SPEED = 35

BoardHeight = 20
BoardWidth  = 10

; Column offset for bounding box
BLOCK_START_X = 5

; These are CHR banks, not tiles
TILE_X = $00
TILE_1 = $01
TILE_2 = $02

PAL_A  = $00
PAL_B  = $40
PAL_C  = $80
PAL_D  = $C0

BlockGridOffset_X = 2
;BlockGridOffset_Y = 1

.enum GS ; Game states
Fall
Place
Clear
Garbage
.endenum

.enum GSArg
Fall = 0
Place = 3
Clear = 5
Garbage = 0
.endenum

CLEAR_FRAMES = 3

.enum Block
Z
S
T
L
J
I
O
.endenum

GamePalettes:
    .byte BG_COLOR, $00, $1A, $20
    ;.byte BG_COLOR, $0F, $1A, $20
    .byte BG_COLOR, $23, $21, $20
    .byte BG_COLOR, $2C, $28, $20
    .byte BG_COLOR, $15, $27, $20

SpritePalettes:
    .byte BG_COLOR, $0F, $0A, $20
    .byte BG_COLOR, $13, $11, $20
    .byte BG_COLOR, $1C, $18, $20
    .byte BG_COLOR, $05, $17, $20

BlockStart_Y:
    .byte 1 ; Z
    .byte 1 ; S
    .byte 1 ; T
    .byte 1 ; L
    .byte 1 ; J
    .byte 0 ; I
    .byte 1 ; O

BlockBg_Tiles:
    .byte TILE_Z
    .byte TILE_S
    .byte TILE_T
    .byte TILE_L
    .byte TILE_J
    .byte TILE_I
    .byte TILE_O

SP_TILE_1 = $11
SP_TILE_2 = $12

; Raw color values for each block
BlockColors:
    .byte $15 ; Z
    .byte $1A ; S
    .byte $23 ; T
    .byte $27 ; L
    .byte $21 ; J
    .byte $2C ; I
    .byte $28 ; O

BlockColors_Place:
    .byte $25 ; Z
    .byte $2A ; S
    .byte $33 ; T
    .byte $37 ; L
    .byte $31 ; J
    .byte $3C ; I
    .byte $38 ; O

BlockColors_Ghost:
.if BG_COLOR = $20
    .byte $25 ; Z
    .byte $2A ; S
    .byte $33 ; T
    .byte $37 ; L
    .byte $31 ; J
    .byte $3C ; I
    .byte $38 ; O
.else
    .byte $05 ; Z
    .byte $0A ; S
    .byte $13 ; T
    .byte $17 ; L
    .byte $11 ; J
    .byte $1C ; I
    .byte $18 ; O
.endif

BlockSprites_Tiles:
    .byte SP_TILE_1 ; Z
    .byte SP_TILE_2 ; S
    .byte SP_TILE_1 ; T
    .byte SP_TILE_2 ; L
    .byte SP_TILE_2 ; J
    .byte SP_TILE_1 ; I
    .byte SP_TILE_2 ; O

SP_PAL_A = PAL_A >> 6
SP_PAL_B = PAL_B >> 6
SP_PAL_C = PAL_C >> 6
SP_PAL_D = PAL_D >> 6

BlockSprites_Palettes:
    .byte SP_PAL_D ; Z
    .byte SP_PAL_A ; S
    .byte SP_PAL_B ; T
    .byte SP_PAL_D ; L
    .byte SP_PAL_B ; J
    .byte SP_PAL_C ; I
    .byte SP_PAL_C ; O

; Offsets for each block in full 8px increments
; Includes rotations
BlockOffsets_Y:
    .byte 0, 0, 1, 1 ; Z, rot 0
    .byte 0, 1, 1, 2 ; Z, rot 1
    .byte 1, 1, 2, 2 ; Z, rot 2
    .byte 0, 1, 1, 2 ; Z, rot 3

    .byte 0, 0, 1, 1 ; S, rot 0
    .byte 0, 1, 1, 2 ; S, rot 1
    .byte 1, 1, 2, 2 ; S, rot 2
    .byte 0, 1, 1, 2 ; S, rot 3

    .byte 0, 1, 1, 1 ; T, rot 0
    .byte 0, 1, 1, 2 ; T, rot 1
    .byte 1, 1, 1, 2 ; T, rot 2
    .byte 0, 1, 1, 2 ; T, rot 3

    .byte 0, 1, 1, 1 ; L, rot 0
    .byte 0, 1, 2, 2 ; L, rot 1
    .byte 1, 1, 1, 2 ; L, rot 2
    .byte 0, 0, 1, 2 ; L, rot 3

    .byte 0, 1, 1, 1 ; J, rot 0
    .byte 0, 0, 1, 2 ; J, rot 1
    .byte 1, 1, 1, 2 ; J, rot 2
    .byte 0, 1, 2, 2 ; J, rot 3

    .byte 1, 1, 1, 1 ; I, rot 0
    .byte 0, 1, 2, 3 ; I, rot 1
    .byte 2, 2, 2, 2 ; I, rot 2
    .byte 0, 1, 2, 3 ; I, rot 3

    ;.byte 1, 1, 2, 2 ; O, rot 0
    ;.byte 1, 1, 2, 2 ; O, rot 1
    ;.byte 1, 1, 2, 2 ; O, rot 2
    ;.byte 1, 1, 2, 2 ; O, rot 3
    .byte 0, 0, 1, 1 ; O, rot 0
    .byte 0, 0, 1, 1 ; O, rot 1
    .byte 0, 0, 1, 1 ; O, rot 2
    .byte 0, 0, 1, 1 ; O, rot 3

; Includes rotations
BlockOffsets_X:
    .byte 0, 1, 1, 2 ; Z, rot 0
    .byte 2, 1, 2, 1 ; Z, rot 1
    .byte 0, 1, 1, 2 ; Z, rot 2
    .byte 1, 0, 1, 0 ; Z, rot 3

    .byte 1, 2, 0, 1 ; S, rot 0
    .byte 1, 1, 2, 2 ; S, rot 1
    .byte 1, 2, 0, 1 ; S, rot 2
    .byte 0, 0, 1, 1 ; S, rot 3

    .byte 1, 0, 1, 2 ; T, rot 0
    .byte 1, 1, 2, 1 ; T, rot 1
    .byte 0, 1, 2, 1 ; T, rot 2
    .byte 1, 0, 1, 1 ; T, rot 3

    .byte 2, 0, 1, 2 ; L, rot 0
    .byte 1, 1, 1, 2 ; L, rot 1
    .byte 0, 1, 2, 0 ; L, rot 2
    .byte 0, 1, 1, 1 ; L, rot 3

    .byte 0, 0, 1, 2 ; J, rot 0
    .byte 1, 2, 1, 1 ; J, rot 1
    .byte 0, 1, 2, 2 ; J, rot 2
    .byte 1, 1, 0, 1 ; J, rot 3

    .byte 0, 1, 2, 3 ; I, rot 0
    .byte 2, 2, 2, 2 ; I, rot 1
    .byte 0, 1, 2, 3 ; I, rot 2
    .byte 1, 1, 1, 1 ; I, rot 3

    .byte 1, 2, 1, 2 ; O, rot 0
    .byte 1, 2, 1, 2 ; O, rot 1
    .byte 1, 2, 1, 2 ; O, rot 2
    .byte 1, 2, 1, 2 ; O, rot 3

; BlockLeft contains the minimum X coordinate
BlockLeft:
    ;.byte 0, 1, 0, 0 ; Z
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-0

    ;.byte 0, 1, 0, 0 ; S
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-0

    ;.byte 0, 1, 0, 0 ; T
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-0

    ;.byte 0, 1, 0, 0 ; L
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-0

    ;.byte 0, 1, 0, 0 ; J
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-0

    ;.byte 0, 2, 0, 1 ; I
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-2
    .byte BlockGridOffset_X-0
    .byte BlockGridOffset_X-1

    ;.byte 1, 1, 1, 1 ; O
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-1
    .byte BlockGridOffset_X-1

; BlockRight contains the maximum X coordinate
BlockRight:
    ;.byte 1, 1, 1, 2 ; Z
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+2

    ;.byte 1, 1, 1, 2 ; S
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+2

    ;.byte 1, 1, 1, 2 ; T
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+2

    ;.byte 1, 1, 1, 2 ; L
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+2

    ;.byte 1, 1, 1, 2 ; J
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+2

    ;.byte 0, 1, 0, 2 ; I
    .byte BoardWidth-BlockGridOffset_X+1+0
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+0
    .byte BoardWidth-BlockGridOffset_X+1+2

    ;.byte 1, 1, 1, 1 ; O
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1
    .byte BoardWidth-BlockGridOffset_X+1+1

; Columns to clear on each frame of the animation
ClearRows_Frames:
    ;.out "ClearRows_Frames"
    .repeat 5, i
        ;.byte 4-i, 5+i
        .byte i, 9-i
        ;.out .sprintf(".byte %d, %d", i, 9-i)
    .endrepeat

.macro KO col, row
    .byte .lobyte($100+((row*-1)*10)+col)
.endmacro

.macro KO_WALL col, row
    ;        X                  Y
    .lobytes $100+col , $100+row*-1
.endmacro

.enum Kick
; S = Spawn
; D = 2x (double)
SL ; 0 -> 3 CCW
SR ; 0 -> 1 CW
RS ; 1 -> 0 CCW
RD ; 1 -> 2 CW
DR ; 2 -> 1 CCW
DL ; 2 -> 3 CW
LD ; 3 -> 2 CCW
LS ; 3 -> 0 CW
.endenum

KickOffsets_Wall:
    ; Spawn -> Left (CCW)
    KO_WALL  1,  0
    KO_WALL  1,  1
    KO_WALL  0, -2
    KO_WALL  1, -2

    ; Spawn -> Right (CW)
    KO_WALL -1,  0
    KO_WALL -1,  1
    KO_WALL  0, -2
    KO_WALL -1, -2

    ; Right -> Spawn (CCW)
    KO_WALL  1,  0
    KO_WALL  1, -1
    KO_WALL  0,  2
    KO_WALL  1,  2

    ; Right -> 2x (CW)
    KO_WALL  1,  0
    KO_WALL  1, -1
    KO_WALL  0,  2
    KO_WALL  1,  2

    ; 2x -> Right (CCW)
    KO_WALL -1,  0
    KO_WALL -1,  1
    KO_WALL  0, -2
    KO_WALL -1, -2

    ; 2x -> Left (CW)
    KO_WALL  1,  0
    KO_WALL  1,  1
    KO_WALL  0, -2
    KO_WALL  1, -2

    ; Left -> 2x (CCW)
    KO_WALL -1,  0
    KO_WALL -1, -1
    KO_WALL  0,  2
    KO_WALL -1,  2

    ; Left -> spawn (CW)
    KO_WALL -1,  0
    KO_WALL -1, -1
    KO_WALL  0,  2
    KO_WALL -1,  2

KickOffsets:
    ; Spawn -> Left (CCW)
    KO  1,  0
    KO  1,  1
    KO  0, -2
    KO  1, -2

    ; Spawn -> Right (CW)
    KO -1,  0
    KO -1,  1
    KO  0, -2
    KO -1, -2

    ; Right -> Spawn (CCW)
    KO  1,  0
    KO  1, -1
    KO  0,  2
    KO  1,  2

    ; Right -> 2x (CW)
    KO  1,  0
    KO  1, -1
    KO  0,  2
    KO  1,  2

    ; 2x -> Right (CCW)
    KO -1,  0
    KO -1,  1
    KO  0, -2
    KO -1, -2

    ; 2x -> Left (CW)
    KO  1,  0
    KO  1,  1
    KO  0, -2
    KO  1, -2

    ; Left -> 2x (CCW)
    KO -1,  0
    KO -1, -1
    KO  0,  2
    KO -1,  2

    ; Left -> spawn (CW)
    KO -1,  0
    KO -1, -1
    KO  0,  2
    KO -1,  2

.if * - KickOffsets <> 4*8
    .error .sprintf("KickOffsets wrong size: %d", (* - KickOffsets))
.else
    .out .sprintf("KickOffsets size: %d", (* - KickOffsets))
.endif

KickOffsets_I_Wall:
    ; Spawn -> Left (CCW)
    KO_WALL -1,  0
    KO_WALL  2,  0
    KO_WALL -1,  2
    KO_WALL  2, -1

    ; Spawn -> Right (CW)
    KO_WALL -2,  0
    KO_WALL  1,  0
    KO_WALL -2, -1
    KO_WALL  1,  2

    ; Right -> Spawn (CCW)
    KO_WALL  2,  0
    KO_WALL -1,  0
    KO_WALL  2,  1
    KO_WALL -1, -2

    ; Right -> 2x (CW)
    KO_WALL -1,  0
    KO_WALL  2,  0
    KO_WALL -1,  2
    KO_WALL  2, -1

    ; 2x -> Right (CCW)
    KO_WALL  1,  0
    KO_WALL -2,  0
    KO_WALL  1, -2
    KO_WALL -2,  1

    ; 2x -> Left (CW)
    KO_WALL  2,  0
    KO_WALL -1,  0
    KO_WALL  2,  1
    KO_WALL -1, -2

    ; Left -> 2x (CCW)
    KO_WALL -2,  0
    KO_WALL  1,  0
    KO_WALL -2, -1
    KO_WALL  1,  2

    ; Left -> spawn (CW)
    KO_WALL  1,  0
    KO_WALL -2,  0
    KO_WALL  1, -2
    KO_WALL -2,  1

KickOffsets_I:
    ; Spawn -> Left (CCW)
    KO -1,  0
    KO  2,  0
    KO -1,  2
    KO  2, -1

    ; Spawn -> Right (CW)
    KO -2,  0
    KO  1,  0
    KO -2, -1
    KO  1,  2

    ; Right -> Spawn (CCW)
    KO  2,  0
    KO -1,  0
    KO  2,  1
    KO -1, -2

    ; Right -> 2x (CW)
    KO -1,  0
    KO  2,  0
    KO -1,  2
    KO  2, -1

    ; 2x -> Right (CCW)
    KO  1,  0
    KO -2,  0
    KO  1, -2
    KO -2,  1

    ; 2x -> Left (CW)
    KO  2,  0
    KO -1,  0
    KO  2,  1
    KO -1, -2

    ; Left -> 2x (CCW)
    KO -2,  0
    KO  1,  0
    KO -2, -1
    KO  1,  2

    ; Left -> spawn (CW)
    KO  1,  0
    KO -2,  0
    KO  1, -2
    KO -2,  1

.if * - KickOffsets_I <> 4*8
    .error .sprintf("KickOffsets_I wrong size: %d", (* - KickOffsets_I))
.else
    .out .sprintf("KickOffsets_I size: %d", (* - KickOffsets_I))
.endif


; Player ID in Y
; AddressPointer1 points to the start of a
; FieldGrid (P1 or P2).
; TmpY is the offset in the field grid that the
; block offsets start.
AlignBlockWithField:
    ; Align to playfield, but we want the offset
    ; (y-1)*10+x-2 = y*10+x-2-10 = y*10+x-12
    lda BlockY, y
    sta MMC5_MultA
    lda #10
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc BlockX, y
    sec
    sbc #12 ; alignment correction (2 col, 1 row)
    sta TmpY

    lda CurrentBlock, y
    asl a
    asl a
    clc
    adc BlockRotation, y
    asl a
    asl a
    tax ; offset in BlockOffsets_X/Y

    cpy #0
    bne :+

    lda #.lobyte(FieldGrid)
    sta AddressPointer1+0
    lda #.hibyte(FieldGrid)
    sta AddressPointer1+1
    jmp :++
:
    lda #.lobyte(FieldGridP2)
    sta AddressPointer1+0
    lda #.hibyte(FieldGridP2)
    sta AddressPointer1+1
:
    rts

VsGameStates:
    .word State_Fall
    .word State_Place
    .word State_Clear
    .word State_Garbage

; PlayerID in Y
DoPlayer:
    lda GameState, y
    asl a
    tax

    lda VsGameStates+0, x
    sta AddressPointer1+0
    lda VsGameStates+1, x
    sta AddressPointer1+1
    jmp (AddressPointer1)

; Add garbage to field
State_Garbage:
    bit CurrentBlock+1
    bmi @2pMode
    ; TODO: check for garbage clear
    jsr NextBlock
    lda #GS::Fall
    sta GameState, y
    lda #0
    sta GameStateArg, y
    rts

@2pMode:
    ; TODO: spawn garbo
    jsr NextBlock
    lda #GS::Fall
    sta GameState, y
    lda #0
    sta GameStateArg, y
    rts

; Clear rows.  should already know which to clear
State_Clear:
    jsr @addrs
    jsr ExpandClearRowIds

    lda GameStateArg, y
    cmp #GSArg::Clear
    bne :+
    ; set rows to white
    tya
    tax
    dec GameStateArg, x
    lda #CLEAR_FRAMES
    sta GameStateArgB, x
    jmp SetClearRows
:

    lda GameStateArg, y
    bpl :+
    jmp @moveDown
:

    tya
    tax
    dec GameStateArgB, x
    beq :+
    rts
:
    lda #CLEAR_FRAMES
    sta GameStateArgB, x

    ; Clear the rows, two columns at a time
    lda GameStateArg, y
    asl a
    tax
    lda ClearRows_Frames, x
    sta TmpA
    lda ClearRows_Frames+1, x
    sta TmpB

    tya
    pha

    lda #10
    sta MMC5_MultB

    ldx #0
@loop:
    lda ClearRows, x
    beq @next

    stx MMC5_MultA
    clc
    lda MMC5_MultA
    adc TmpA
    tay
    lda #0
    sta (AddressPointer1), y
    clc
    lda MMC5_MultA
    adc TmpB
    tay
    lda #0
    sta (AddressPointer1), y

@next:
    inx
    cpx #.sizeof(ClearRows)
    bne @loop

    pla
    tay
    tax
    dec GameStateArg, x
    rts

@addrs:
    cpy #0
    bne :+
    lda #.lobyte(ClearRowIdsP1)
    sta AddressPointer2+0
    lda #.hibyte(ClearRowIdsP1)
    sta AddressPointer2+1

    ; dest
    lda #.lobyte(FieldGrid)
    sta AddressPointer1+0
    lda #.hibyte(FieldGrid)
    sta AddressPointer1+1

    jmp :++
:
    lda #.lobyte(ClearRowIdsP2)
    sta AddressPointer2+0
    lda #.hibyte(ClearRowIdsP2)
    sta AddressPointer2+1

    ; dest
    lda #.lobyte(FieldGridP2)
    sta AddressPointer1+0
    lda #.hibyte(FieldGridP2)
    sta AddressPointer1+1
:
    rts

@moveDown:
    tya
    tax
    dec GameStateArgB, x
    beq :+
    rts
:

    tya
    pha

    lda BlockY, y
    sec
    sbc #1
    sta TmpA

    ; find lowest row
    ldx #.sizeof(ClearRows)-1
:   lda ClearRows, x
    bne :+
    dex
    bpl :-
:
    stx TmpA ; First row index to clear
    stx TmpB ; Source row to copy from

@top:
    ; Destination
    ldx TmpA
    bmi @bottom
    stx MMC5_MultA
    lda #10
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc AddressPointer1+0
    sta AddressPointer3+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer3+1

:
    dec TmpB
    bpl :+
    jmp @ClearWithEmpty
:

    ; multiple rows?
    ldx TmpB
    lda ClearRows, x
    bne :--

    ; Source
    lda TmpB
    sta MMC5_MultA

    clc
    lda MMC5_MultA
    adc AddressPointer1+0
    sta AddressPointer4+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer4+1

    jsr VsDoClearRow

@bottom:
    dec TmpA
    bpl @top

    pla
    tay

    lda #GS::Garbage
    sta GameState, y
    lda #0
    sta GameStateArg, y
    rts

; Clear the top row(s)
@ClearWithEmpty:
    ldy #0
    lda #0
:   sta (AddressPointer3), y
    iny
    cpy #10
    bne :-
    jmp @bottom

VsDoClearRow:
    ldy #0
    .repeat 10, i
        lda (AddressPointer4), y
        sta (AddressPointer3), y
        .if i < 9
            iny
        .endif
    .endrepeat
    rts

; Expands an array of four bytes to an array of 20
; (eg, ClearRowIdsP1 -> ClearRows)
ExpandClearRowIds:
    tya
    pha

    lda BlockY, y
    sta TmpY
    dec TmpY

    ldx #0
    lda #0
:
    sta ClearRows, x
    inx
    cpx #.sizeof(ClearRows)
    bne :-

    ldy #0
:
    lda (AddressPointer2), y
    bmi @next
    clc
    adc TmpY
    tax
    lda #1
    sta ClearRows, x
@next:
    iny
    cpy #4
    bne :-

    pla
    tay
    rts

; Turn rows that are going to be cleared white
SetClearRows:
    bit CurrentBlock+1
    bpl :+
    lda CurrentGameMode+GameMode::BaseType
    cmp #GameBaseType::Standard
    bne :+
    lda CurrentGameMode+GameMode::TypeArg
    cmp #GameStandardArgs::DirtyBoard
    bne :+
    jsr CountDirtyClear
:
    tya
    pha

    lda BlockY, y
    sec
    sbc #1
    sta TmpA

    lda #10
    sta MMC5_MultB

    ldy #0
    sty TmpY
@loop:
    ldy TmpY
    lda (AddressPointer2), y
    bmi @next

    clc
    adc TmpA
    sta MMC5_MultA
    lda MMC5_MultA
    tay

    ldx #10
    lda #$03
@setLoop:
    sta (AddressPointer1), y
    iny
    dex
    bne @setLoop

@next:
    inc TmpY
    lda TmpY
    cmp #4
    bne @loop

    pla
    tay
    rts

; place a block, discover rows to clear
State_Place:
    ldx CurrentBlock, y
    lda BlockColors_Place, x
    cpy #0
    bne @p2
    sta Palettes+(4*4)+1
    sta Palettes+(4*4)+2
    sta Palettes+(4*4)+3
    jmp :+
@p2:
    sta Palettes+(4*5)+1
    sta Palettes+(4*5)+2
    sta Palettes+(4*5)+3
:

    tya
    tax
    dec GameStateArg, x
    bmi :+
    rts
:

    ; Reset highlight color
    lda GamePalettes+3
    cpy #0
    bne :+
    sta Palettes+(4*4)+3
    jmp :++
:
    sta Palettes+(4*5)+3
:

    jsr PlaceBlock

    jsr CheckClearRows
    beq :+
    lda #GS::Clear
    sta GameState, y
    lda #GSArg::Clear
    sta GameStateArg, y
    rts

:
    lda #GS::Garbage
    sta GameState, y
    lda #0
    sta GameStateArg, y
    rts

CheckClearRows:
    jsr AlignBlockWithField

    cpy #0
    bne :+
    lda #.lobyte(ClearRowIdsP1)
    sta AddressPointer2+0
    lda #.hibyte(ClearRowIdsP1)
    sta AddressPointer2+1
    jmp :++
:
    lda #.lobyte(ClearRowIdsP2)
    sta AddressPointer2+0
    lda #.hibyte(ClearRowIdsP2)
    sta AddressPointer2+1
:

    lda BlockY, y
    sta TmpY
    dec TmpY

    tya
    pha

    lda #$FF
    ldy #0
    .repeat 4, i
        sta (AddressPointer2), y
        .if i < 3
            iny
        .endif
    .endrepeat

    ldy #0
    lda BlockOffsets_Y+0, x
    sta (AddressPointer2), y

    .repeat 3, i
    lda BlockOffsets_Y+1+i, x
    cmp (AddressPointer2), y
    beq :+
    iny
    sta (AddressPointer2), y
:
    .endrepeat

    lda #10
    sta MMC5_MultB

    ldy #0
    sty TmpX ; count to clear
    sty TmpZ
@checkLoop:
    ;lda ClearRowIds, x
    lda (AddressPointer2), y
    bmi @next

    clc
    adc TmpY

    sta MMC5_MultA
    lda MMC5_MultA
    sta TmpA
    tay
    ldx #0
@rowLoop:
    lda (AddressPointer1), y
    beq @nope
    iny
    inx
    cpx #10
    bne @rowLoop

    inc TmpX

    ; Put the offset to the start of the field into
    ; this array so we don't have to calculate it later
    ; ...maybe
    ;ldy TmpZ
    ;lda TmpA
    ;sta (AddressPointer2), y

    jmp @next

@nope:
    lda #$FF
    ldy TmpZ
    ;sta ClearRowIds, x
    sta (AddressPointer2), y

@next:
    inc TmpZ
    ldy TmpZ
    cpy #4
    bne @checkLoop

    pla
    tay

    asl a
    tax

    lda TmpX
    clc
    adc Lines, x
    sta Lines, x
    lda Lines+1, x
    adc #0
    sta Lines+1, x

    lda TmpX
    sta ClearCount, y
    bne :+
    lda #$FF
    sta Combo, y
    lda #0
    rts
:
    clc
    tya
    tax
    inc Combo, x
    lda #1
    rts

; Acts on player input, applies gravity
State_Fall:
    bit CurrentBlock+1
    bpl :+
    lda CurrentGameMode+GameMode::TypeArg
    cmp #GameStandardArgs::NoHold
    beq @noHold
:
    lda #BUTTON_SELECT ; select
    jsr ButtonPressed
    beq :+
    jmp SwapHeldPiece
:
@noHold:

    ; check for released first
    lda #BUTTON_DOWN ; down
    jsr ButtonReleased
    beq :+
    lda #0
    sta SoftDrop, y
:

    lda #BUTTON_DOWN ; down, pressed
    jsr ButtonPressed
    beq :+
    lda #1
    sta SoftDrop, y
    jmp @btnVertDone

:   lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq @btnVertDone
    jmp DoHardDrop
@btnVertDone:

    lda #BUTTON_RIGHT | BUTTON_LEFT ; left | right
    jsr ButtonReleased
    beq :+
    lda #$FF
    sta RepeatRight, y
    sta RepeatLeft, y
:

    lda RepeatLeft, y
    bmi :+
    tya
    tax
    dec RepeatLeft, x
    bpl :+
    lda Option_ShiftRepeat
    sta RepeatLeft, y
    jmp @btnLeft
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq @noLeft
    lda Option_ShiftStart
    sta RepeatLeft, y
@btnLeft:
    tya
    tax
    dec BlockX, x
    bpl :+
    lda #0
    sta BlockX, x

    ; No kicks in left/right stuff
:   jsr CheckCollide_Walls
    beq :+
    tya
    tax
    inc BlockX, x
    jmp @horizDone

:   jsr CheckCollide_Grid
    beq @horizDone
    tya
    tax
    inc BlockX, x

@noLeft:
    lda RepeatRight, y
    bmi :+
    tya
    tax
    dec RepeatRight, x
    bpl :+
    lda Option_ShiftRepeat
    sta RepeatRight, y
    jmp @btnRight
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq @horizDone
    lda Option_ShiftStart
    sta RepeatRight, y
@btnRight:
    tya
    tax
    inc BlockX, x
    lda BlockX, x
    cmp #BoardWidth+1 ; BoardWidth+1
    bcc :+
    lda #BoardWidth ; BoardWidth
    sta BlockX, x

:   jsr CheckCollide_Walls
    beq :+
    tya
    tax
    dec BlockX, x

:   jsr CheckCollide_Grid
    beq @horizDone
    tya
    tax
    dec BlockX, x
@horizDone:

    lda #BUTTON_A
    jsr ButtonPressed
    beq @noA
    tya
    tax
    lda BlockRotation, x
    asl a
    ora #1
    sta TmpA
    inc BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x

    jsr CheckCollide_Kicks
    beq @rotDone
    ; Rotation collides; rotate back
    tya
    tax
    dec BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x
    jmp @rotDone

@noA:
    lda #BUTTON_B
    jsr ButtonPressed
    beq @rotDone
    tya
    tax
    lda BlockRotation, x
    asl a
    sta TmpA
    dec BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x

    jsr CheckCollide_Kicks
    beq @rotDone
    ; Rotation collides; rotate back
    tya
    tax
    inc BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x

@rotDone:

    lda SoftDrop, y
    beq @noSoft
    sec
    lda DropSpeed, y
    sbc #SOFT_SPEED
    sta DropSpeed, y
    bpl @noDrop
    jmp @doDrop

@noSoft:

    tya
    tax
    dec DropSpeed, x
    bpl @noDrop
@doDrop:
    lda SoftDrop, y
    beq :+
    lda #1
    sta DropScore ; for 1P mode.  ignored in 2P
:
    tya
    tax
    inc BlockY, x
    lda Speed_Drop
    sta DropSpeed, y
    jsr CheckCollide_Grid
    beq @noDrop
    tya
    tax
    dec BlockY, x
    lda #GS::Place
    sta GameState, y
    lda #GSArg::Place
    sta GameStateArg, y

@noDrop:
    rts

DoHardDrop:
    lda #$FF
    sta RepeatRight, y
    sta RepeatLeft, y

    bit CurrentBlock+1
    bpl :+
    jsr @Sp
:
    lda GhostY, y
    sta BlockY, y

    lda #GS::Place
    sta GameState, y
    lda #GSArg::Place
    sta GameStateArg, y
    rts

@Sp:
    sec
    lda GhostY
    sbc CurrentY
    asl a
    clc
    adc DropScore
    sta DropScore
    lda Option_ScreenShake
    beq :+
    lda #ShakeTable_Length-1
    sta DropShake
:
    rts

CheckCollide_Kicks:
    ;tya ; save player ID
    ;pha

    ; TmpA has Kick enum value
    lda TmpA
    asl a
    asl a
    sta TmpA ; KickOffsets index
    asl a
    sta TmpZZ ; kickoffsets_wall index

    lda BlockX, y
    sta TmpB

    lda BlockY, y
    sta TmpC

    ; No kick
    jsr CheckCollide_Walls
    bne :+
    jsr CheckCollide_Grid
    bne :+
    jmp @done
:

    jsr AlignBlockWithField
    stx TmpXX

    lda TmpY
    sta TmpZ

    lda #0
    sta Debug_Kick

@wtf:
    ; the I block has different offsets
    lda CurrentBlock, y
    cmp #Block::I
    bne :+
    lda #.lobyte(KickOffsets_I)
    sta AddressPointer1+0
    lda #.hibyte(KickOffsets_I)
    sta AddressPointer1+1

    lda #.lobyte(KickOffsets_I_Wall)
    sta AddressPointer2+0
    lda #.hibyte(KickOffsets_I_Wall)
    sta AddressPointer2+1
    jmp :++
:
    lda #.lobyte(KickOffsets)
    sta AddressPointer1+0
    lda #.hibyte(KickOffsets)
    sta AddressPointer1+1

    lda #.lobyte(KickOffsets_Wall)
    sta AddressPointer2+0
    lda #.hibyte(KickOffsets_Wall)
    sta AddressPointer2+1
:

    sty TmpYY

    .repeat 4, i
        lda TmpZ
        ldx TmpA
        clc
        ;adc KickOffsets, x
        ldy #0
        adc (AddressPointer1), y
        sta TmpY

        clc
        ldx TmpZZ
        ;lda KickOffsets_Wall+0, x
        ldy #0
        lda (AddressPointer2), y
        adc TmpB
        ldy TmpYY
        sta BlockX, y

        clc
        ;lda KickOffsets_Wall+1, x
        ldy #1
        lda (AddressPointer2), y
        adc TmpC
        ldy TmpYY
        sta BlockY, y

        jsr CheckCollide_Walls
        bne :+

        ldx TmpXX
        jsr CheckCollide_Grid_AfterAlign
        bne :+
        jmp @done
:
        .if i < 3
            inc TmpA
            inc TmpZZ
            inc TmpZZ
        .endif
        inc Debug_Kick
    .endrepeat

    ldy TmpYY

;@fail:
    lda TmpB
    sta BlockX, y
    lda TmpC
    sta BlockY, y
    lda #1 ; fail
    rts

@done:
    lda #0
    rts

; Player in Y
CheckCollide_Walls:
    lda CurrentBlock, y
    asl a
    asl a
    clc
    adc BlockRotation, y
    tax

    lda BlockX, y
    cmp BlockLeft, x
    bcs :+
    lda #1 ; collides with left edge
    rts
:

    lda BlockX, y
    cmp BlockRight, x
    bcc :+
    lda #1
    rts

:   lda #0
    rts

; Player in Y
CheckCollide_Grid:
    jsr AlignBlockWithField

CheckCollide_Grid_AfterAlign:
    tya ; save player ID
    pha

    lda #10
    sta MMC5_MultB

    .repeat 4, i
    clc
    lda BlockOffsets_Y+i, x
    sta MMC5_MultA

    lda MMC5_MultA
    adc BlockOffsets_X+i, x
    ; A contains offset of tile under inpsection

    adc TmpY
    cmp #(BoardWidth*BoardHeight)
    bcc :+
    jmp @collide
:
    tay
    lda (AddressPointer1), y
    beq :+
    jmp @collide
:
    .endrepeat

    pla
    tay
    lda #0
    rts

@collide:
    pla
    tay
    lda #1
    rts ; collision

PlaceBlock:
    tya
    pha
    ldx CurrentBlock, y
    lda BlockBg_Tiles, x
    sta TmpA

    jsr AlignBlockWithField
    ; X has offset into BlockOffsets tables

    lda #4
    sta TmpX
    lda #10
    sta MMC5_MultB

@loop:
    lda BlockOffsets_Y, x
    sta MMC5_MultA

    clc
    lda MMC5_MultA
    adc BlockOffsets_X, x
    adc TmpY
    ; A contains offset of tile under inpsection

    tay
    lda TmpA
    sta (AddressPointer1), y

    inx
    dec TmpX
    bne @loop

    lda #0
    sta HeldSwapped
    pla
    tay
    rts

; PlayerID in Y
CalculateGhost:
    lda BlockY, y
    pha
    clc
    adc #1
    sta BlockY, y

    jsr AlignBlockWithField
    stx TmpXX

    jsr CheckCollide_Grid_AfterAlign
    beq :+
    pla
    sta BlockY, y
    sta GhostY, y
    rts
:

@loop:
    lda BlockY, y
    clc
    adc #1
    sta BlockY, y
    lda TmpY
    clc
    adc #10
    sta TmpY

    ldx TmpXX
    jsr CheckCollide_Grid_AfterAlign
    bne @collide
    jmp @loop

@collide:
    sec
    lda BlockY, y
    sbc #1
    sta GhostY, y
    pla
    sta BlockY, y
    rts

NextBlock:
    lda #$00
    sta HeldSwapped, y

NextBlock_Swap:
    lda Speed_Drop, y
    sta DropSpeed, y

    bit CurrentBlock+1
    bpl :+ ; no single block for 2P mode
    lda CurrentGameMode+GameMode::BaseType
    cmp #GameBaseType::SingleBlock
    bne :+
    lda CurrentGameMode+GameMode::TypeArg
    sta CurrentBlock
    jmp @afterBag
:

    cpy #0
    bne @p2
    lda #.lobyte(BagP1)
    sta AddressPointer1+0
    lda #.hibyte(BagP1)
    sta AddressPointer1+1
    jmp :+
@p2:
    lda #.lobyte(BagP2)
    sta AddressPointer1+0
    lda #.hibyte(BagP2)
    sta AddressPointer1+1
:

    tya
    pha
    tax
    ldy #0
    lda (AddressPointer1), y
    sta CurrentBlock, x

    ldy #1
@bumpLoop:
    lda (AddressPointer1), y
    dey
    sta (AddressPointer1), y
    iny
    iny
    cpy #14
    bne @bumpLoop
    dey
    lda #$FF
    sta (AddressPointer1), y

    pla
    tay

    tya
    tax
    dec BagLeft, x
    bne :+
    jsr ShuffleBag
:

@afterBag:
    lda #BLOCK_START_X
    sta BlockX, y

    ldx CurrentBlock, y
    lda BlockStart_Y, x
    sta BlockY, y
    sta CurrentY, y

    lda #0
    sta BlockRotation, y
    sta SoftDrop, y

    lda #$FF
    sta RepeatRight, y
    sta RepeatLeft, y

    jsr CheckCollide_Grid
    bne :+
    rts
:

    bit CurrentBlock+1
    bpl @2p
    jmp DedTransition
@2p:
    jmp VsModeGameOver

; Player ID in Y
SwapHeldPiece:
    lda HeldSwapped, y
    beq :+
    rts
:
    lda #$FF
    sta HeldSwapped, y

    lda HoldPiece, y
    bpl @swap
    ; nothing was held
    lda CurrentBlock, y
    sta HoldPiece, y
    jmp NextBlock_Swap

@swap:
    lda HoldPiece, y
    pha
    lda CurrentBlock, y
    sta HoldPiece, y
    pla
    sta CurrentBlock, y

    lda #BLOCK_START_X
    sta BlockX, y

    ldx CurrentBlock, y
    lda BlockStart_Y, x
    sta BlockY, y

    lda #0
    sta BlockRotation, y
    sta SoftDrop, y

    lda #$FF
    sta RepeatRight, y
    sta RepeatLeft, y
    rts

; Init's both player's bags
ShuffleBag_Init:
    ldy #Player1
    lda #.lobyte(BagP1)
    sta AddressPointer1+0
    lda #.hibyte(BagP1)
    sta AddressPointer1+1
    jsr _shuffle_clr

    clc
    lda AddressPointer1+0
    adc #7
    sta AddressPointer1+0

    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
    jsr _shuffle_clr

    ldy #Player2
    lda #.lobyte(BagP2)
    sta AddressPointer1+0
    lda #.hibyte(BagP2)
    sta AddressPointer1+1
    jsr _shuffle_clr

    clc
    lda AddressPointer1+0
    adc #7
    sta AddressPointer1+0

    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
    jmp _shuffle_clr


ShuffleBag:
    cpy #0
    bne @p2
    lda #.lobyte(BagP1+7)
    sta AddressPointer1+0
    lda #.hibyte(BagP1+7)
    sta AddressPointer1+1
    jmp :+
@p2:
    lda #.lobyte(BagP2+7)
    sta AddressPointer1+0
    lda #.hibyte(BagP2+7)
    sta AddressPointer1+1
:

_shuffle_clr:
    lda #7
    sta BagLeft, y

    tya
    pha

    lda #$FF
    ldy #0
@clear:
    sta (AddressPointer1), y
    iny
    cpy #7
    bne @clear

    lda #0
    sta TmpX

@loop:
    inc rng_index
    ldx rng_index
    lda PieceRng, x

    ldy #0
@check:
    cmp (AddressPointer1), y
    beq @loop
    iny
    cpy #7
    bne @check

    ldy TmpX
    sta (AddressPointer1), y
    inc TmpX
    cpy #6
    bne @loop

    pla
    tay
    rts


.endif
