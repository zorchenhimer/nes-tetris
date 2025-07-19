.ifndef _GAME_RAM_
_GAME_RAM_ = 1

; TODO:
;   - Mini T-Spins.  They aren't currently
;     differentiated from standard T-Spins.

.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 2
BlockRotation: .res 2
DropSpeed: .res 2
DebugRotation: .res 1
RotationDirection: .res 2
TSpin: .res 2
BlockX: .res 2
BlockY: .res 2

KickTable: .res 1
KickTableWall: .res 1
KickX: .res 2
KickY: .res 2
KickStep: .res 1
KickFieldOffset: .res 1

KickWallX:  .res 1
KickAlignX: .res 1

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

Combo: .res 2 ; one byte per player
ClearCount: .res 2 ; rows cleared this frame

.segment "BSS"

CurrentX: .res 2
CurrentY: .res 2
GhostY: .res 2
GhostX: .res 1 ; debug only

PlaceWait: .res 2

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

Lines: .res 4 ; two bytes per player
UpdateCombo: .res 2
Back2Back: .res 2

TmpScore: .res 3

; Contains tile indicies
HighScore: .res 6

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

; Index into GarbagePerLine (GarbageLines enum)
LastClearType: .res 2

LastClearCount: .res 2

VsGarbageP1_Tiles: .res 12
VsGarbageP2_Tiles: .res 12

VsGarbageP1_Attr: .res 12
VsGarbageP2_Attr: .res 12

GarbageCounter: .res 2 ; frames until next ready
GarbageReady:   .res 2 ; next group ready to spawn

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

GarbageBarP1_Tile_Empty = $07
GarbageBarP2_Tile_Empty = $05

GarbageBarP1_Tile_EmptyBottom = $03
GarbageBarP2_Tile_EmptyBottom = $04

GarbageBarP1_Tile_Bottom = $F2
GarbageBarP2_Tile_Bottom = $F6

GarbageBarP1_Tile = $F0
GarbageBarP2_Tile = $F4

GarbageBar_Attr_Empty   = $02
GarbageBar_Attr_Pending = $C1
GarbageBar_Attr_Ready   = $C2

; Timer is frames, not seconds
GarbageTimerStart = 150

GarbageBarP1_Addr = $21E2
GarbageBarP2_Addr = $21FD

.enum GarbageLines
Single
Double
Triple
Tetris

TSpinSingle
TSpinDouble
TSpinTriple

MiniTSpinSingle
MiniTSpinDouble

PerfectClear
.endenum

.enum Rot
None
CW
CCW
.endenum

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
    .byte BG_COLOR, $2C, $27, $20
    .byte BG_COLOR, $15, $28, $20

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

; Cells to check for a block when in CheckTSpin
TSpinOffsets_Y:
    .byte   0,   0, 20, 20
    .byte   0,   0, 20, 20
    .byte $F6, $F6, 10, 10
    .byte   0,   0, 20, 20

TSpinOffsets_X:
    .byte   0, 2,   0, 2
    .byte $FF, 1, $FF, 1
    .byte   0, 2,   0, 2
    .byte   0, 2,   0, 2

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

.macro KO col, row ; value is offset into field
    .byte .lobyte($100+((row*-1)*10)+col)
.endmacro

.macro KO_WALL col, row ; values are col, row into offset
    ;        X                  Y
    .lobytes $100+col , $100+(row*-1)
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

KickOffsets_Wall_CCW:
    ; Spawn -> Left (CCW)
    KO_WALL  0,  0
    KO_WALL  1,  0
    KO_WALL  1,  1
    KO_WALL  0, -2
    KO_WALL  1, -2

    ; Right -> Spawn (CCW)
    KO_WALL  0,  0
    KO_WALL  1,  0
    KO_WALL  1, -1
    KO_WALL  0,  2
    KO_WALL  1,  2

    ; 2x -> Right (CCW)
    KO_WALL  0,  0
    KO_WALL -1,  0
    KO_WALL -1,  1
    KO_WALL  0, -2
    KO_WALL -1, -2

    ; Left -> 2x (CCW)
    KO_WALL  0,  0
    KO_WALL -1,  0
    KO_WALL -1, -1
    KO_WALL  0,  2
    KO_WALL -1,  2

KickOffsets_Wall_CW:
    ; Spawn -> Right (CW)
    KO_WALL  0,  0
    KO_WALL -1,  0
    KO_WALL -1,  1
    KO_WALL  0, -2
    KO_WALL -1, -2

    ; Right -> 2x (CW)
    KO_WALL  0,  0
    KO_WALL  1,  0
    KO_WALL  1, -1
    KO_WALL  0,  2
    KO_WALL  1,  2

    ; 2x -> Left (CW)
    KO_WALL  0,  0
    KO_WALL  1,  0
    KO_WALL  1,  1
    KO_WALL  0, -2
    KO_WALL  1, -2

    ; Left -> spawn (CW)
    KO_WALL  0,  0
    KO_WALL -1,  0
    KO_WALL -1, -1
    KO_WALL  0,  2
    KO_WALL -1,  2

KickOffsets_CCW:
    ; Spawn -> Left (CCW)
    KO  0,  0
    KO  1,  0
    KO  1,  1
    KO  0, -2
    KO  1, -2

    ; Right -> Spawn (CCW)
    KO  0,  0
    KO  1,  0
    KO  1, -1
    KO  0,  2
    KO  1,  2

    ; 2x -> Right (CCW)
    KO  0,  0
    KO -1,  0
    KO -1,  1
    KO  0, -2
    KO -1, -2

    ; Left -> 2x (CCW)
    KO  0,  0
    KO -1,  0
    KO -1, -1
    KO  0,  2
    KO -1,  2

KickOffsets_CW:
    ; Spawn -> Right (CW)
    KO  0,  0
    KO -1,  0
    KO -1,  1
    KO  0, -2
    KO -1, -2

    ; Right -> 2x (CW)
    KO  0,  0
    KO  1,  0
    KO  1, -1
    KO  0,  2
    KO  1,  2

    ; 2x -> Left (CW)
    KO  0,  0
    KO  1,  0
    KO  1,  1
    KO  0, -2
    KO  1, -2

    ; Left -> spawn (CW)
    KO  0,  0
    KO -1,  0
    KO -1, -1
    KO  0,  2
    KO -1,  2

KickOffsets_I_Wall_CCW:
    ; Spawn -> Left (CCW)
    KO_WALL  0,  0
    KO_WALL -1,  0
    KO_WALL  2,  0
    KO_WALL -1,  2
    KO_WALL  2, -1

    ; Right -> Spawn (CCW)
    KO_WALL  0,  0
    KO_WALL  2,  0
    KO_WALL -1,  0
    KO_WALL  2,  1
    KO_WALL -1, -2

    ; 2x -> Right (CCW)
    KO_WALL  0,  0
    KO_WALL  1,  0
    KO_WALL -2,  0
    KO_WALL  1, -2
    KO_WALL -2,  1

    ; Left -> 2x (CCW)
    KO_WALL  0,  0
    KO_WALL -2,  0
    KO_WALL  1,  0
    KO_WALL -2, -1
    KO_WALL  1,  2

KickOffsets_I_Wall_CW:
    ; Spawn -> Right (CW)
    KO_WALL  0,  0
    KO_WALL -2,  0
    KO_WALL  1,  0
    KO_WALL -2, -1
    KO_WALL  1,  2

    ; Right -> 2x (CW)
    KO_WALL  0,  0
    KO_WALL -1,  0
    KO_WALL  2,  0
    KO_WALL -1,  2
    KO_WALL  2, -1

    ; 2x -> Left (CW)
    KO_WALL  0,  0
    KO_WALL  2,  0
    KO_WALL -1,  0
    KO_WALL  2,  1
    KO_WALL -1, -2

    ; Left -> spawn (CW)
    KO_WALL  0,  0
    KO_WALL  1,  0
    KO_WALL -2,  0
    KO_WALL  1, -2
    KO_WALL -2,  1

KickOffsets_I_CCW:
    ; Spawn -> Left (CCW)
    KO  0,  0
    KO -1,  0
    KO  2,  0
    KO -1,  2
    KO  2, -1

    ; Right -> Spawn (CCW)
    KO  0,  0
    KO  2,  0
    KO -1,  0
    KO  2,  1
    KO -1, -2

    ; 2x -> Right (CCW)
    KO  0,  0
    KO  1,  0
    KO -2,  0
    KO  1, -2
    KO -2,  1

    ; Left -> 2x (CCW)
    KO  0,  0
    KO -2,  0
    KO  1,  0
    KO -2, -1
    KO  1,  2

KickOffsets_I_CW:
    ; Spawn -> Right (CW)
    KO  0,  0
    KO -2,  0
    KO  1,  0
    KO -2, -1
    KO  1,  2

    ; Right -> 2x (CW)
    KO  0,  0
    KO -1,  0
    KO  2,  0
    KO -1,  2
    KO  2, -1

    ; 2x -> Left (CW)
    KO  0,  0
    KO  2,  0
    KO -1,  0
    KO  2,  1
    KO -1, -2

    ; Left -> spawn (CW)
    KO  0,  0
    KO  1,  0
    KO -2,  0
    KO  1, -2
    KO -2,  1

; Input:
;   Player ID in Y
; Output:
;   AddressPointer1 points to the start of a
;       FieldGrid (P1 or P2).
;   TmpY is the offset in the field grid that the
;       active block starts.
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
    rts

:   lda #.lobyte(FieldGridP2)
    sta AddressPointer1+0
    lda #.hibyte(FieldGridP2)
    sta AddressPointer1+1
    rts

VsGameStates:
    .word State_Fall
    .word State_Place
    .word State_Clear
    .word State_Garbage

; PlayerID in Y
DoPlayer:
    lda IsPaused
    beq :+
    rts
:

    lda GarbageCounter, y
    cmp #$FF
    beq :+
    tya
    tax
    dec GarbageCounter, x
    lda GarbageCounter, y
    cmp #$FF
    bne :+
    lda #1
    sta GarbageReady, y
:

    jsr UpdateGarbageBar

    lda GameState, y
    asl a
    tax

    lda VsGameStates+0, x
    sta AddressPointer1+0
    lda VsGameStates+1, x
    sta AddressPointer1+1
    jsr @jmp
    rts

@jmp:
    jmp (AddressPointer1)

; Add garbage to field
State_Garbage:
    lda #0
    sta TSpin, y

    bit CurrentBlock+1
    bpl @2pMode
    jsr NextBlock
    lda #GS::Fall
    sta GameState, y
    lda #0
    sta GameStateArg, y
    rts

@2pMode:
    sty TmpY

    lda ClearCount, y
    beq :+
    jmp @end
:

    ;
    ; Spawn Garbo lines
    lda GarbageReady, y
    bne :+
    jmp @end
:

    cpy #Player2
    beq :+
    lda #.lobyte(VsGarbageP1_Tiles)
    sta AddressPointer1+0
    lda #.hibyte(VsGarbageP1_Tiles)
    sta AddressPointer1+1

    lda #.lobyte(VsGarbageP1_Attr)
    sta AddressPointer2+0
    lda #.hibyte(VsGarbageP1_Attr)
    sta AddressPointer2+1

    lda #.lobyte(FieldGrid)
    sta AddressPointer3+0
    lda #.hibyte(FieldGrid)
    sta AddressPointer3+1

    lda #GarbageBarP1_Tile_EmptyBottom ; GarbageBarP1_Tile_EmptyBottom
    sta TmpX
    pha
    lda #GarbageBarP1_Tile ; GarbageBarP1_Tile
    sta TmpXX
    lda #GarbageBarP1_Tile_Empty
    pha
    jmp :++
:
    lda #.lobyte(VsGarbageP2_Tiles)
    sta AddressPointer1+0
    lda #.hibyte(VsGarbageP2_Tiles)
    sta AddressPointer1+1

    lda #.lobyte(VsGarbageP2_Attr)
    sta AddressPointer2+0
    lda #.hibyte(VsGarbageP2_Attr)
    sta AddressPointer2+1

    lda #.lobyte(FieldGridP2)
    sta AddressPointer3+0
    lda #.hibyte(FieldGridP2)
    sta AddressPointer3+1

    lda #GarbageBarP2_Tile_EmptyBottom ; GarbageBarP2_Tile_EmptyBottom
    sta TmpX
    pha
    lda #GarbageBarP2_Tile ; GarbageBarP2_Tile
    sta TmpXX
    lda #GarbageBarP2_Tile_Empty
    pha
:

    ;
    ; Make sure the bar isn't empty
    ldy #.sizeof(VsGarbageP1_Tiles)-1
    lda (AddressPointer1), y
    cmp TmpX
    bne :+
    pla
    pla
    jmp @end
:

    ;lda TmpA
    ;sta (AddressPointer2), y

    ;
    ; Count number of rows
    ldx #1
:   dey
    lda (AddressPointer1), y
    cmp TmpXX ; not start of next or empty
    bne :+
    ;lda TmpA
    ;sta (AddressPointer2), y
    inx
    jmp :-
:

    txa
    pha
    ldy TmpY
    jsr SpawnGarbage

    pla
    sta TmpX

    ; update waiting garbo
    lda #0
    sta GarbageReady, y

    sec
    lda AddressPointer1+0
    sbc TmpX
    sta AddressPointer3+0
    lda AddressPointer1+1
    sbc #0
    sta AddressPointer3+1

    ldy #11
:   lda (AddressPointer3), y
    sta (AddressPointer1), y
    dey
    cpy TmpX
    bcs :-

    pla
    sta TmpA ; GarbageBarP1_Tile_Empty
    pla
    sta TmpB ; GarbageBarP2_Tile_EmptyBottom

    ; TmpXX = GarbageBarP2_Tile

    lda TmpA
:   sta (AddressPointer1), y
    dey
    bpl :-

    ldy #11
    lda (AddressPointer1), y
    cmp TmpA ; _Tile_Empty
    bne @notEmpty

    lda TmpB
    sta (AddressPointer1), y
    jmp @end

@notEmpty:
    ldy TmpY
    lda #GarbageTimerStart
    sta GarbageCounter, y

@end:
    ldy TmpY
    jsr NextBlock
    lda #GS::Fall
    sta GameState, y
    lda #0
    sta GameStateArg, y
    rts

; Checks if current player is in a TSpin location
CheckTSpin:
    lda CurrentBlock, y
    cmp #Block::T
    beq :+
    rts
:

    ; This bit should only be set if the player just
    ; rotated the piece
    lda TSpin, y
    and #$80
    bne :+
    rts
:

    lda #0
    sta TSpin, y
    sta TmpX ; occupied count

    jsr AlignBlockWithField

    sty TmpYY

    ; Check for walls & floor.  Add two if the long
    ; side of the piece is up against one.
    lda BlockRotation, y
    cmp #1 ; left wall
    bne @checkFloor

    lda BlockX, y
    cmp #1
    bne @notWall

    lda #2
    sta TmpX

    clc
    lda TmpY
    adc #2
    tay
    lda (AddressPointer1), y
    beq :+
    inc TmpX
:
    clc
    lda TmpY
    adc #2+20 ; 2+20
    tay
    lda (AddressPointer1), y
    beq @wallDone
    inc TmpX
    jmp @wallDone

@checkFloor:
    cmp #0 ; floor
    bne @checkRight

    lda BlockY, y
    cmp #19
    bne @notWall

    lda #2
    sta TmpX

    ldy TmpY
    lda (AddressPointer1), y
    beq :+
    inc TmpX
:
    clc
    lda TmpY
    adc #2
    tay
    lda (AddressPointer1), y
    beq @wallDone
    inc TmpX

    jmp @wallDone

@checkRight:
    cmp #3 ; right wall
    bne @notWall

    lda BlockX, y
    cmp #10
    bne @notWall

    lda #2
    sta TmpX

    clc
    lda TmpY
    adc #2
    tay
    lda (AddressPointer1), y
    beq :+
    inc TmpX
:
    clc
    lda TmpY
    adc #2+20 ; 2+20
    tay
    lda (AddressPointer1), y
    bne @wallDone
    inc TmpX
    jmp @wallDone

@notWall:

    ldx #0
@loop:
    clc
    lda TmpY
    adc TSpinOffsets_Y, x
    clc
    adc TSpinOffsets_X, x
    tay
    lda (AddressPointer1), y
    beq @next
    inc TmpX

@next:
    inx
    cpx #4
    bne @loop

@wallDone:
    ldy TmpYY

    lda #0
    sta TSpin, y

    lda TmpX
    cmp #3
    bcc :+
    lda #1
    sta TSpin, y
:
    rts

; TmpA has rows cleared
; Return in A: 0 for false, 1 for true
CheckPerfectClear:
    sec
    lda #20-1
    sbc TmpA
    sta MMC5_MultA
    lda #10
    sta MMC5_MultB

    tya
    pha

    ldy MMC5_MultA
    ldx #10
@search:
    lda (AddressPointer1), y
    bne @nope
    iny
    dex
    bne @search
    pla
    tay
    lda #1
    rts

@nope:
    pla
    tay
    lda #0
    rts

; Clear rows.  should already know which to clear
State_Clear:
    jsr @addrs
    jsr ExpandClearRowIds

    lda TmpA
    sta LastClearCount, y

    lda GameStateArg, y
    cmp #GSArg::Clear
    bne @notFirst
    ; first frame of clear

    lda TSpin, y
    and #$7F
    bne @tspin
    lda TmpA
    ;lda GarbagePerLine, x
    sta LastClearType, y
    jmp :+

@tspin:
    lda TmpA
    clc
    adc #GarbageLines::TSpinSingle
    ;tax
    ;lda GarbagePerLine, x
    sta LastClearType, y

:

    lda LastClearType, y
    cmp #GarbageLines::Tetris
    bcc @noB2B

    ldx Back2Back, y
    inx
    cpx #1
    bcc :+
    ldx #1
:   txa
    sta Back2Back, y
    jmp :+

@noB2B:
    lda #$FF
    sta Back2Back, x
:

    jsr CheckPerfectClear
    beq :+

    lda LastClearType, y
    ora #$80
    sta LastClearType, y

:

    tya
    tax
    dec LastClearType, x

    ; set rows to white
    tya
    tax
    dec GameStateArg, x
    lda #CLEAR_FRAMES
    sta GameStateArgB, x

    lda LastClearCount, y
    ora #$80
    sta LastClearCount, y

    jmp SetClearRows

@notFirst:
    lda GameStateArg, y ; state of clear
    bpl :+
    jmp @moveDown
:

    tya
    tax
    dec GameStateArgB, x ; countdown for next animation
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

    ; LastClearType includes T-Spin offsets as well as
    ; setting the high bit on Perfect Clear.
    lda LastClearType, y
    bpl :+
    lda #GarbageLines::PerfectClear ; GarbageLines::PerfectClear
:
    tax

    ; Combo is not covered in the LastClearType value
    lda GarbagePerLine, x
    ldx Combo, y
    cpx #GarbageComboTableSize
    bcc :+
    ; Larger than table, use last value in table
    ldx GarbageComboTableSize-1
:   clc
    adc GarbagePerCombo, x
    beq @noGarbage

    ; TODO (maybe?):
    ;   Check for queued garbage.  if it exists,
    ;   subtract the number of rows we would have
    ;   sent to the other player from the queue.
    ;   If this number is negative (ie, clear more
    ;   queued rows than are actively queued), send
    ;   the remainder to the other player.


    tax
    jsr QueueGarbage
@noGarbage:

    jsr NextBlock
    lda #GS::Fall
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
; (eg, ClearRowIdsP1 (AddressPointer2) -> ClearRows)
; TmpA contains number of rows
ExpandClearRowIds:
    tya
    pha

    lda #0
    sta TmpA

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
    inc TmpA
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
    jsr CheckTSpin

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
    lda #0
    sta TSpin, y
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
    lda TSpin, y
    pha

    lda #0
    sta TSpin, y

    tya
    tax
    dec BlockX, x

    ; No kicks in left/right stuff
    jsr CheckCollide_Walls
    beq :+ ; no collide
    ; collide
    pla
    sta TSpin, y
    tya
    tax
    inc BlockX, x
    jmp @horizDone

:   jsr CheckCollide_Grid
    bne :+
    pla ; no Collide, discard TSpin value
    jmp @horizDone
:
    pla
    sta TSpin, y
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
    lda TSpin, y
    pha

    lda #0
    sta TSpin, y

    tya
    tax
    inc BlockX, x

    jsr CheckCollide_Walls
    beq :+
    ; collide
    pla
    sta TSpin, y

    tya
    tax
    dec BlockX, x
    jmp @horizDone

:   jsr CheckCollide_Grid
    beq :+
    pla
    sta TSpin, y
    jmp :++

:   pla ; no collide, discard TSpin value
    jmp @horizDone

:   tya
    tax
    dec BlockX, x
@horizDone:

    lda #BUTTON_A
    jsr ButtonPressed
    beq @noA
    lda #$80
    sta TSpin, y
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

    lda #Rot::CW
    sta RotationDirection, y

    jsr CheckCollide_Kicks
    beq @aOK
    ; Rotation collides; rotate back
    tya
    tax
    dec BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x
    jmp @rotDone

@aOK:
    ; reset wait counter
    lda #$80
    sta PlaceWait, y
    jmp @rotDone

@noA:
    lda #BUTTON_B
    jsr ButtonPressed
    beq @rotDone
    lda #$80
    sta TSpin, y
    tya
    tax
    lda BlockRotation, x
    asl a
    sta TmpA
    dec BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x

    lda #Rot::CCW
    sta RotationDirection, y

    jsr CheckCollide_Kicks
    beq @bOK
    ; Rotation collides; rotate back
    tya
    tax
    inc BlockRotation, x
    lda #$03
    and BlockRotation, x
    sta BlockRotation, x
    jmp @rotDone

@bOK:
    ; reset wait counter
    lda #$80
    sta PlaceWait, y

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

    ; "hover" substate?  don't place block immediately
    ; if high bit set, wait isn't active.
    ; this is a disgusting hack.  do this
    ; shit correctly, lmao
    tya
    tax
    lda PlaceWait, y
    bpl :+
    dec BlockY, x
    lda #1
    sta PlaceWait, y
    jmp @noDrop

:   beq @place
    dec BlockY, x
    dec PlaceWait, x
    bne @noDrop

    inc BlockY, x
    lda #$80
    sta PlaceWait, x

@place:
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
    jsr @singlePlayer
:
    sec
    lda GhostY, y
    sbc BlockY, y
    beq :+
    ; Hard Drops that drop any rows will clear TSpin
    ; bonuses.
    ; TSpins should not be cancled if the hard drop
    ; just skips the lock-in (ie, hard drop with nowhere
    ; to go).
    lda #0
    sta TSpin, y
:

    lda GhostY, y
    sta BlockY, y

    lda #GS::Place
    sta GameState, y
    lda #GSArg::Place
    sta GameStateArg, y
    rts

@singlePlayer:
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
    tya ; save player ID
    pha

    ldx RotationDirection, y
    lda CurrentBlock, y
    cmp #Block::I
    bne @notI
    ; I Block
    cpx #Rot::CW ; Rot::CW
    bne :+
    ; KickOffsets_I_CW
    lda #.lobyte(KickOffsets_I_CW)
    sta AddressPointer2+0
    lda #.hibyte(KickOffsets_I_CW)
    sta AddressPointer2+1

    ; KickOffsets_I_Wall_CW
    lda #.lobyte(KickOffsets_I_Wall_CW)
    sta AddressPointer3+0
    lda #.hibyte(KickOffsets_I_Wall_CW)
    sta AddressPointer3+1

    lda #255
    sta TmpX
    jmp @addrDone

:   ; KickOffsets_I_CCW
    lda #.lobyte(KickOffsets_I_CCW)
    sta AddressPointer2+0
    lda #.hibyte(KickOffsets_I_CCW)
    sta AddressPointer2+1

    ; KickOffsets_I_Wall_CCW
    lda #.lobyte(KickOffsets_I_Wall_CCW)
    sta AddressPointer3+0
    lda #.hibyte(KickOffsets_I_Wall_CCW)
    sta AddressPointer3+1

    lda #1
    sta TmpX
    jmp @addrDone

@notI:
    cpx #Rot::CW ; Rot::CW
    bne :+
    ; KickOffsets_CW
    lda #.lobyte(KickOffsets_CW)
    sta AddressPointer2+0
    lda #.hibyte(KickOffsets_CW)
    sta AddressPointer2+1

    ; KickOffsets_Wall_CW
    lda #.lobyte(KickOffsets_Wall_CW)
    sta AddressPointer3+0
    lda #.hibyte(KickOffsets_Wall_CW)
    sta AddressPointer3+1

    lda #255
    sta TmpX
    jmp @addrDone

:   ; KickOffsets_CCW
    lda #.lobyte(KickOffsets_CCW)
    sta AddressPointer2+0
    lda #.hibyte(KickOffsets_CCW)
    sta AddressPointer2+1

    ; KickOffsets_Wall_CCW
    lda #.lobyte(KickOffsets_Wall_CCW)
    sta AddressPointer3+0
    lda #.hibyte(KickOffsets_Wall_CCW)
    sta AddressPointer3+1

    lda #1
    sta TmpX
@addrDone:

    ; Get offsets into both tables (non-wall and wall)
    clc
    lda BlockRotation, y
    adc TmpX
    and #$03
    ;sta KickRotation

    ;lda BlockRotation, y
    sta MMC5_MultA

    lda #5
    sta MMC5_MultB

    lda MMC5_MultA
    sta KickTable      ; table is one byte entries
    asl a
    sta KickTableWall ; table is two byte entries

    lda BlockX, y
    sta KickX

    lda BlockY, y
    sta KickY

    jsr AlignBlockWithField
    stx KickAlignX
    lda TmpY
    sta KickFieldOffset

    ; AddressPointer1 - FieldGrid
    ; AddressPointer2 - KickOffsets
    ; AddressPointer3 - KickOffsets_Wall

    lda #0
    sta KickStep

    lda CurrentBlock, y
    asl a
    asl a
    clc
    adc BlockRotation, y
    sta KickWallX
    ;tax

@kickLoop:
    lda KickStep
    asl a
    clc
    adc KickTableWall
    tay

    ldx KickWallX
    clc
    lda KickX+0
    adc (AddressPointer3), y
    cmp BlockLeft, x
    bcc @next
    cmp BlockRight, x
    bcs @next

    sta KickX+1 ; "good" X

    iny
    lda KickY+0
    adc (AddressPointer3), y
    sta KickY+1

    lda KickStep
    clc
    adc KickTable
    tay
    clc
    lda KickFieldOffset
    adc (AddressPointer2), y
    sta TmpY

    ldx KickAlignX
    jsr CheckCollide_Grid_AfterAlign
    beq @pass

@next:
    inc KickStep
    lda KickStep
    cmp #5
    bne @kickLoop

@fail: ; fail
    pla
    tay
    lda #1
    rts

@pass:
    pla
    tay

    lda KickX+1
    sta BlockX, y
    lda KickY+1
    sta BlockY, y

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

; check each block in the active piece for a coordinate
; that's past the wall bounds.
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
    .ifdef DEBUG_PIECE
    lda #DEBUG_PIECE
    .endif
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

UpdateGarbageBar:
;    lda GarbageReady, y
;    bne :+
;    rts
;:

    cpy #Player2
    beq :+
    lda #.lobyte(VsGarbageP1_Tiles)
    sta AddressPointer1+0
    lda #.hibyte(VsGarbageP1_Tiles)
    sta AddressPointer1+1

    lda #.lobyte(VsGarbageP1_Attr)
    sta AddressPointer2+0
    lda #.hibyte(VsGarbageP1_Attr)
    sta AddressPointer2+1

    lda #GarbageBarP1_Tile_EmptyBottom
    sta TmpX ; GarbageBarP1_Tile_EmptyBottom
    lda #GarbageBarP1_Tile
    sta TmpXX
    jmp :++
:
    lda #.lobyte(VsGarbageP2_Tiles)
    sta AddressPointer1+0
    lda #.hibyte(VsGarbageP2_Tiles)
    sta AddressPointer1+1

    lda #.lobyte(VsGarbageP2_Attr)
    sta AddressPointer2+0
    lda #.hibyte(VsGarbageP2_Attr)
    sta AddressPointer2+1
    lda #GarbageBarP2_Tile_EmptyBottom
    sta TmpX
    lda #GarbageBarP2_Tile
    sta TmpXX
:
    sty TmpY

    lda GarbageReady, y
    beq :+
    ; garbage is ready.  turn red
    lda #GarbageBar_Attr_Ready ; GarbageBar_Attr_Ready
    sta TmpA
    jmp :++
:
    ; garbage isn't ready, turn yellow
    lda #GarbageBar_Attr_Pending ; GarbageBar_Attr_Pending
    sta TmpA
:

    ;
    ; Make sure the bar isn't empty
    ldy #.sizeof(VsGarbageP1_Tiles)-1
    lda (AddressPointer1), y
    cmp TmpX
    beq @done

    lda TmpA
    sta (AddressPointer2), y

    ;
    ; Count number of rows
    ldx #1
:   dey
    lda (AddressPointer1), y
    cmp TmpXX ; not start of next or empty
    bne :+
    lda TmpA
    sta (AddressPointer2), y
    inx
    jmp :-
:


@done:
    ldy TmpY
    rts

; Player ID in Y
; Rows to spawn in X
SpawnGarbage:
    stx TmpX
    sty TmpY

    stx MMC5_MultA
    lda #10
    sta MMC5_MultB
    ldx MMC5_MultA
    stx TmpXX ; cells to check
    txa

    clc
    adc AddressPointer3+0
    sta AddressPointer4+0

    lda AddressPointer3+1
    adc #0
    sta AddressPointer4+1

    lda #0
    sta TmpA ; game over?

    sec
    lda #BoardHeight
    sbc TmpX
    sta MMC5_MultB
    lda #10
    sta MMC5_MultA
    lda MMC5_MultA
    sta TmpB

    ldy #0
@findLoop:
    lda (AddressPointer3), y
    bne @gameOver
    iny
    dex
    bne @findLoop

    ldy #0
@copyLoop:
    lda (AddressPointer4), y
    sta (AddressPointer3), y
    iny
    cpy TmpB
    ;cpy #BoardHeight * BoardWidth
    bne @copyLoop

    lda #$01
@spawn:
    sta (AddressPointer3), y
    iny
    cpy #200
    bne @spawn

    lda rng_index
    and #$07
    clc
    adc #1
    sta TmpB

    sec
    lda #199
    sbc TmpB
    tay

    lda #0
@holePunch:
    sta (AddressPointer3), y
    .repeat 10
    dey
    .endrepeat
    dec TmpX
    bne @holePunch

@gameOver:

    ldy TmpY
    rts

GarbagePerLine:
    .byte 0 ; single
    .byte 1 ; double
    .byte 2 ; triple
    .byte 4 ; tetris / quad

    .byte 2 ; T-Spin Single
    .byte 4 ; T-Spin Double
    .byte 6 ; T-Spin Triple

    .byte 0 ; mini T-Spin single
    .byte 1 ; mini T-Spin double

    .byte 10 ; Perfect Clear. good luck, lmao

GarbagePerLineTableSize = * - GarbagePerLine

GarbagePerCombo:
    .byte 0 ; 0 combo

    ; 1, 2
    .byte 1
    .byte 1

    ; 3, 4
    .byte 2
    .byte 2

    ; 5, 6
    .byte 3
    .byte 3

    ; 7, 8, 9
    .byte 4
    .byte 4
    .byte 4

    ; 10+
    .byte 5
GarbageComboTableSize = * - GarbagePerCombo

.enum ClearText
Single
Double
Triple
Quadruple

MiniTSpin
TSpin
Back2Back
PerfectClear
.endenum
.endif
