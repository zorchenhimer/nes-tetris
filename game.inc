.ifndef _GAME_RAM_
_GAME_RAM_ = 1

.pushseg
.segment "ZEROPAGE"
CurrentBlock: .res 2
BlockRotation: .res 2
DropSpeed: .res 1

SoftDrop: .res 2
HardDrop: .res 2

Level_Tiles: .res 4
Score_Tiles: .res 8
Lines_Tiles: .res 6
Combo_Tiles: .res 2

TmpM: .res 3

MathA: .res 3
MathB: .res 2
MathC: .res 2

BottomVals: .res 4
TopVals: .res 4
LowestRows: .res 4

TmpBlockOffset: .res 1

BlockGrid: .res 4*4
BlockGridP2: .res 4*4

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

BagLeft: .res 2 ; 2p
BagA: .res 7 ; next pieces
BagB: .res 7 ; next pieces
HoldPiece: .res 1

FieldGrid: .res 10*20
FieldGridP2: .res 10*20

HeldSwapped: .res 2

Level: .res 2

; one player per byte
Combo: .res 2
UpdateCombo: .res 2

TmpScore: .res 3

; Contains tile indicies
HighScore: .res 6

ClearCount: .res 1 ; rows cleared this frame
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

GamePalettes:
    .byte $20, $0F, $1A, $20
    .byte $20, $23, $21, $20
    .byte $20, $2C, $28, $20
    .byte $20, $15, $27, $20

SpritePalettes:
    .byte $20, $0F, $0A, $20
    .byte $20, $13, $11, $20
    .byte $20, $1C, $18, $20
    .byte $20, $05, $17, $20

BlockStart_Y:
    .byte 1 ; Z
    .byte 1 ; S
    .byte 1 ; T
    .byte 1 ; L
    .byte 1 ; J
    .byte 0 ; I
    .byte 0 ; O

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

    .byte 1, 1, 2, 2 ; O, rot 0
    .byte 1, 1, 2, 2 ; O, rot 1
    .byte 1, 1, 2, 2 ; O, rot 2
    .byte 1, 1, 2, 2 ; O, rot 3

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


; Align to playfield, but we want the offset
; (y-1)*10+x-2 = y*10+x-2-10 = y*10+x-12
GetBlockOffset:
    sta MMC5_MultA
    lda #10
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc TmpX
    sec
    sbc #12 ; alignment correction
    rts

; Player ID in Y
; AddressPointer1 points to the upper left corner
; of the block grid in the field.
AlignBlockWithField:
    lda BlockX, y
    sta TmpX
    lda BlockY, y
    jsr GetBlockOffset
    sta TmpY

    lda CurrentBlock, y
    asl a
    asl a
    clc
    adc BlockRotation, y
    asl a
    asl a
    tax

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

.endif
