.include "game.inc"

Player1 = 0
Player2 = 1

VsBlockLocation_X = 24 - (8*2)
VsBlockLocation_Y = 55 - (8*1)

InitVsMode:
    lda #.lobyte(GamePalettes)
    sta AddressPointer1+0
    lda #.hibyte(GamePalettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes

    lda #.lobyte(GamePalettes)
    sta AddressPointer1+0
    lda #.hibyte(GamePalettes)
    sta AddressPointer1+1
    jsr LoadSpPalettes
    ;lda #.lobyte(GameOverPalette)
    ;sta AddressPointer1+0
    ;lda #.hibyte(GameOverPalette)
    ;sta AddressPointer1+1
    ;ldx #7
    ;jsr LoadPalette

    lda #0
    ldy #0
:
    .ifdef DEBUG_FIELD
    lda DebugField, y
    .endif
    sta FieldGrid, y
    sta FieldGridP2, y
    iny
    cpy #200
    bne :-

    jsr InitBagsVs
    jsr NextBlockP1
    jsr NextBlockP2

    clc
    lda BlockY+0
    adc #2
    sta BlockY+0

    clc
    lda BlockY+1
    adc #2
    sta BlockY+1

    lda #.lobyte(NmiVsGame)
    sta NmiHandler+0
    lda #.hibyte(NmiVsGame)
    sta NmiHandler+1

    lda #%1000_0000
    sta PpuControl
    sta $2000

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

    lda #$00
    sta ScrollX
    sta ScrollY
    sta $2005
    sta $2005

    jsr WaitForNMI

    SetIRQ 2, IrqVsGame_Unrolled
    ;SetIRQ 2, IrqVsGame

VsModeFrame:
    jsr ReadControllers

    ; DEBUG
    lda Controller+0
    sta Controller+1
    lda Controller_Old+0
    sta Controller_Old+1

    ldy #Player1

    lda #BUTTON_UP
    jsr ButtonPressed
    beq :+
    dec BlockY+0
    bpl :+
    inc BlockY+0
:

    lda #BUTTON_DOWN
    jsr ButtonPressed
    beq :+
    inc BlockY
    cmp #19
    bne :+
    dec BlockY
:

    lda BlockY+0
    sta BlockY+1

    lda #%0101_1110
    sta $2001
    ldy #Player1
    jsr DoPlayer
    lda #%0011_1110
    sta $2001
    ldy #Player2
    jsr DoPlayer

    lda #%1001_1110
    sta $2001
    jsr UpdateActiveBlocks
    lda #%0001_1110
    sta $2001

    jsr WaitForIRQ
    jmp VsModeFrame


NmiVsGame:
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

IrqVsGame_Unrolled:
    .repeat 20, row
        .repeat 10, cell
            lda FieldGrid+cell+(row*10)
            sta VsPlayfieldStartP1+cell+(row*32)

            lda FieldGridP2+cell+(row*10)
            sta VsPlayfieldStartP2+cell+(row*32)
        .endrepeat
    .endrepeat
    rts

IrqVsGame:
    ;
    ; Draw playfield, p1
    lda #.lobyte(FieldGrid)
    sta AddressPointer2+0
    lda #.hibyte(FieldGrid)
    sta AddressPointer2+1

    ldx #0
@loopRowP1:
    lda PlayfieldPpuRows_VsP1_Lo, x
    sta AddressPointer1+0
    lda PlayfieldPpuRows_VsP1_Hi, x
    sta AddressPointer1+1

    ldy #0
    .repeat 10
    lda (AddressPointer2), y
    sta (AddressPointer1), y
    iny
    .endrepeat

    clc
    lda AddressPointer2+0
    adc #10
    sta AddressPointer2+0
    lda AddressPointer2+1
    adc #0
    sta AddressPointer2+1

    inx
    cpx #20
    bne @loopRowP1

    ;
    ; Draw playfield, p2
    lda #.lobyte(FieldGridP2)
    sta AddressPointer2+0
    lda #.hibyte(FieldGridP2)
    sta AddressPointer2+1

    ldx #0
@loopRowP2:
    lda PlayfieldPpuRows_VsP2_Lo, x
    sta AddressPointer1+0
    lda PlayfieldPpuRows_VsP2_Hi, x
    sta AddressPointer1+1

    ldy #0
    .repeat 10
    lda (AddressPointer2), y
    sta (AddressPointer1), y
    iny
    .endrepeat

    clc
    lda AddressPointer2+0
    adc #10
    sta AddressPointer2+0
    lda AddressPointer2+1
    adc #0
    sta AddressPointer2+1

    inx
    cpx #20
    bne @loopRowP2
    rts

; PlayerID in Y
DoPlayer:
    lda #BUTTON_SELECT ; select
    jsr ButtonPressed
    beq :+
    ldy #Player1
    jmp SwapHeldPiece
:

    ; check for released first
    lda #BUTTON_DOWN
    jsr ButtonReleased
    beq :+
    lda #0
    sta SoftDrop
:

    lda #BUTTON_DOWN ; down, pressed
    jsr ButtonPressed
    beq :+
    lda #1
    sta SoftDrop
    jmp @btnVertDone

:   lda #BUTTON_UP
    jsr ButtonPressed
    beq @btnVertDone
    ;ldy #Player1
    jmp VsHardDrop
@btnVertDone:

    ; TODO: verify this works properly
    lda #BUTTON_RIGHT | BUTTON_LEFT
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

    lda #BUTTON_LEFT
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

    lda #BUTTON_RIGHT
    jsr ButtonPressed
    beq @horizDone
    lda Option_ShiftStart
    sta RepeatRight, y
@btnRight:
    tya
    tax
    inc BlockX, x
    lda BlockX, x
    cmp #BoardWidth+1
    bcc :+
    lda #BoardWidth
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
    rts

VsHardDrop:
    lda #$FF
    sta RepeatRight, y
    sta RepeatLeft, y
    rts

; BagA and BagB are for players 1 and 2 in this mode
InitBagsVs:
    lda #7
    sta BagLeft
    lda #$FF
    ldx #0

:   sta BagA, x
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

NextBlockP1:
    lda #0
    sta HeldSwapped+0

NextBlockP1_Swap:
    lda Speed_Drop
    sta DropSpeed+0

    dec BagLeft+0
    bne :+
    ;jsr ShuffleBagP1
:

    lda BagA+0
    sta CurrentBlock+0

    ; bump a single bag
    ldx #0
:
    lda BagA+1, x
    sta BagA, x
    inx
    cpx #6
    bne :-

    .ifdef DEBUG_PIECE
    lda #DEBUG_PIECE
    sta CurrentBlock+0
    .endif

    lda #BLOCK_START_X
    sta BlockX+0

    ldy CurrentBlock+0
    lda BlockStart_Y, y
    sta BlockY+0
    sta CurrentY+0

    lda #0
    sta BlockRotation+0
    sta SoftDrop+0

    .ifdef DEBUG_ROTATION
    lda #DEBUG_ROTATION
    sta BlockRotation+0
    .endif

    lda #$FF
    sta RepeatRight+0
    sta RepeatLeft+0

    jsr LoadBlockP1
    ;jsr CheckCollideP1
    beq :+
    jmp VsModeGameOver
:   rts

NextBlockP2:
    lda #0
    sta HeldSwapped+1

NextBlockP2_Swap:
    lda Speed_Drop
    sta DropSpeed+1

    dec BagLeft+1
    bne :+
    ;jsr ShuffleBagP2
:

    lda BagB+0
    sta CurrentBlock+1

    ; bump a single bag
    ldx #0
:
    lda BagB+1, x
    sta BagB, x
    inx
    cpx #6
    bne :-

    .ifdef DEBUG_PIECE
    lda #DEBUG_PIECE
    sta CurrentBlock+1
    .endif

    lda #BLOCK_START_X
    sta BlockX+1

    ldy CurrentBlock+1
    lda BlockStart_Y, y
    sta BlockY+1
    sta CurrentY+1

    lda #0
    sta BlockRotation+1
    sta SoftDrop+1

    .ifdef DEBUG_ROTATION
    lda #DEBUG_ROTATION
    sta BlockRotation+1
    .endif

    lda #$FF
    sta RepeatRight+1
    sta RepeatLeft+1

    jsr LoadBlockP2
    ;jsr CheckCollideP2
    beq :+
    jmp VsModeGameOver
:   rts

LoadBlockP1:
    lda CurrentBlock+0
    asl a
    tax

    lda BlockTiles+0, x
    sta AddressPointer1+0
    lda BlockTiles+1, x
    sta AddressPointer1+1

    lda BlockRotation+0
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

LoadBlockP2:
    lda CurrentBlock+1
    asl a
    tax

    lda BlockTiles+0, x
    sta AddressPointer1+0
    lda BlockTiles+1, x
    sta AddressPointer1+1

    lda BlockRotation+1
    asl a
    asl a
    asl a
    asl a
    tay

    ldx #0
@loop:
    lda (AddressPointer1), y
    sta BlockGridP2, x
    iny
    inx
    cpx #16
    bne @loop
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

    .repeat 4, i
        lda TmpZ
        ldx TmpA
        clc
        adc KickOffsets, x
        sta TmpY

        clc
        ldx TmpZZ
        lda KickOffsets_Wall+0, x
        adc TmpB
        sta BlockX, y

        clc
        lda KickOffsets_Wall+1, x
        adc TmpC
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

    lda #4
    sta TmpX ; cell/loop count
    lda #10
    sta MMC5_MultB

@loop:
    clc
    lda BlockOffsets_Y, x
    sta MMC5_MultA

    lda MMC5_MultA
    adc BlockOffsets_X, x
    ; A contains offset of tile under inpsection

    adc TmpY
    tay
    lda (AddressPointer1), y
    beq :+
    pla
    tay
    lda #1
    rts ; collision
:

    inx
    dec TmpX
    bne @loop

    pla
    tay
    lda #0
    rts

PlaceBlock:
; FIXME: preserve Y
    ldx CurrentBlock, y
    lda BlockBg_Tiles, x
    sta TmpA

    jsr AlignBlockWithField

    lda #4
    sta TmpX
    lda #10
    sta MMC5_MultB

@loop:
    clc
    lda BlockOffsets_Y, x
    sta MMC5_MultA

    lda MMC5_MultA
    adc BlockOffsets_X, x
    ; A contains offset of tile under inpsection

    tay
    lda TmpA
    sta (AddressPointer1), y

    inx
    dec TmpX
    bne @loop

    lda #0
    sta HeldSwapped
    rts

; Updates both players at once
UpdateActiveBlocks:
    tya
    pha

    ; P1 first
    ldx CurrentBlock+0
    lda BlockSprites_Palettes, x
    sta TmpA

    lda BlockSprites_Tiles, x
    sta TmpB

    lda CurrentBlock+0
    asl a
    asl a
    clc
    adc BlockRotation+0
    asl a
    asl a
    tax

    lda BlockX+0
    asl a
    asl a
    asl a
    sta TmpX

    lda BlockY+0
    asl a
    asl a
    asl a
    sta TmpY

    .repeat 4, i
        lda BlockOffsets_Y, x
        tay
        lda VsBlockGridLocationY, y
        clc
        adc TmpY
        sta SpriteP1+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda VsBlockGridLocationX, y
        clc
        adc TmpX
        sta SpriteP1+3+(i*4)

        lda TmpA
        sta SpriteP1+2+(i*4)

        lda TmpB
        sta SpriteP1+1+(i*4)

        .if i < 3
            inx
        .endif
    .endrepeat

    ; P2 second
    ldx CurrentBlock+1
    lda BlockSprites_Palettes, x
    sta TmpA

    lda BlockSprites_Tiles, x
    sta TmpB

    lda CurrentBlock+1
    asl a
    asl a
    clc
    adc BlockRotation+1
    asl a
    asl a
    tax

    lda BlockX+1
    asl a
    asl a
    asl a
    ora #$80
    sta TmpX

    lda BlockY+1
    asl a
    asl a
    asl a
    sta TmpY

    .repeat 4, i
        lda BlockOffsets_Y, x
        tay
        lda VsBlockGridLocationY, y
        clc
        adc TmpY
        sta SpriteP2+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda VsBlockGridLocationX, y
        clc
        adc TmpX
        sta SpriteP2+3+(i*4)

        lda TmpA
        sta SpriteP2+2+(i*4)

        lda TmpB
        sta SpriteP2+1+(i*4)

        .if i < 3
            inx
        .endif
    .endrepeat

    pla
    tay
    rts

VsModeGameOver:
    jmp VsModeGameOver

VsPlayfieldStartP1 = $20E3 + MMC5_OFFSET
VsPlayfieldStartP2 = $20F3 + MMC5_OFFSET

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

PlayfieldPpuRows_VsP1_Hi:
    .repeat BoardHeight, i
        .byte .hibyte(VsPlayfieldStartP1+(i*32))
    .endrepeat

PlayfieldPpuRows_VsP1_Lo:
    .repeat BoardHeight, i
        .byte .lobyte(VsPlayfieldStartP1+(i*32))
    .endrepeat

PlayfieldPpuRows_VsP2_Hi:
    .repeat BoardHeight, i
        .byte .hibyte(VsPlayfieldStartP2+(i*32))
    .endrepeat

PlayfieldPpuRows_VsP2_Lo:
    .repeat BoardHeight, i
        .byte .lobyte(VsPlayfieldStartP2+(i*32))
    .endrepeat

VsBlockGridLocationY:
    .repeat 21, i
        .byte VsBlockLocation_Y+(i*8)
    .endrepeat

VsBlockGridLocationX:
    .repeat 11, i
        .byte VsBlockLocation_X+(i*8)
    .endrepeat

