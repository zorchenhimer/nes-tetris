.include "game.inc"

Player1 = 0
Player2 = 1

InitVsMode:
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

    SetIRQ 2, IrqVsGame

VsModeFrame:
    jsr ReadControllers

    jsr DoPlayer1
    jsr DoPlayer2

    jsr WaitForIRQ
    jmp VsModeFrame


NmiVsGame:
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

DoPlayer1:
    lda #BUTTON_SELECT ; select
    jsr ButtonPressed
    beq :+
    jmp SwapHeldPiece
:
    rts

DoPlayer2:
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
    sta CurrentBlock

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
    sta CurrentBlock
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
    sta CurrentBlock

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
    sta CurrentBlock
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

; Player in Y
CheckCollide_Walls:
    lda CurrentBlock, y
    asl a
    asl a
    clc
    adc BlockRotation, y
    tax

    lda BlockX
    cmp BlockLeft, x
    bcs :+
    lda #1 ; collides with left edge
    rts
:

    lda BlockX
    cmp BlockRight, x
    bcc :+
    lda #1
    rts

:   lda #0
    rts

; Player in Y
CheckCollide_Grid:
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
    lda (AddressPointer1), y
    beq :+
    rts ; collision
:

    inx
    dec TmpX
    bne @loop

    lda #0
    rts

PlaceBlock:
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

VsModeGameOver:
    jmp VsModeGameOver

VsPlayfieldStartP1 = $20E3 + MMC5_OFFSET
VsPlayfieldStartP2 = $20F3 + MMC5_OFFSET

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
