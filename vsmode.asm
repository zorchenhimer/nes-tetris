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

    lda #%0000_0010
    sta $5104
    lda #$02

    .repeat 4, i
        ; HOLD/NEXT headers
        sta $2043+MMC5_OFFSET+i
        sta $2049+MMC5_OFFSET+i

        sta $2053+MMC5_OFFSET+i
        sta $2059+MMC5_OFFSET+i

        ; COMBO
        sta $21CE+MMC5_OFFSET+i
        sta $22AE+MMC5_OFFSET+i

        ; LINE
        sta $222E+MMC5_OFFSET+i
        sta $230E+MMC5_OFFSET+i

    .endrepeat

    ; Borders
    lda #$01
    ; tops
    .repeat 12, i
        sta $20C2+MMC5_OFFSET+i
        sta $20D2+MMC5_OFFSET+i
        sta $2362+MMC5_OFFSET+i
        sta $2372+MMC5_OFFSET+i
    .endrepeat

    ; sides
    .repeat 20, i
        sta $20E2+MMC5_OFFSET+(i*32)
        sta $20ED+MMC5_OFFSET+(i*32)
        sta $20F2+MMC5_OFFSET+(i*32)
        sta $20FD+MMC5_OFFSET+(i*32)
    .endrepeat

    ; text boxes
    .repeat 4, i
        sta $21AE+MMC5_OFFSET+i
        sta $226E+MMC5_OFFSET+i
        sta $228E+MMC5_OFFSET+i
        sta $234E+MMC5_OFFSET+i
    .endrepeat

    lda #%0000_0001
    sta $5104

    lda #GS::Fall
    sta GameState+0
    sta GameState+1

    lda #0
    sta GameStateArg+0
    sta GameStateArg+1

    lda DropSpeeds+0
    sta Speed_Drop

    lda #$FF
    sta HoldPiece+0
    sta HoldPiece+1
    sta Combo+0
    sta Combo+1

    jsr ShuffleBag_Init

    ldy #Player1
    jsr NextBlock
    ldy #Player2
    jsr NextBlock

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

VsModeFrame:
    jsr ReadControllers

    ; DEBUG
    ;lda Controller+0
    ;sta Controller+1
    ;lda Controller_Old+0
    ;sta Controller_Old+1

    .ifdef DEBUG_COLORS
        lda #%0101_1110
        sta $2001
    .endif
    ldy #Player1
    jsr DoPlayer
    .ifdef DEBUG_COLORS
        lda #%0011_1110
        sta $2001
    .endif
    ldy #Player2
    jsr DoPlayer

    .ifdef DEBUG_COLORS
        lda #%1001_1110
        sta $2001
    .endif
    jsr UpdateActiveBlocks_Vs
    .ifdef DEBUG_COLORS
        lda #%0001_1110
        sta $2001
    .endif

    lda Combo+0
    cmp #255
    beq :+
    jsr BinToDec_8bit
    .repeat 3, i
        lda bcdOutput+i
        sta Combo_TilesP1+i
    .endrepeat
    jmp :++
:
    lda #' '
    sta Combo_TilesP1+0
    sta Combo_TilesP1+1
    lda #'0'
    sta Combo_TilesP1+2
:

    lda Combo+1
    cmp #255
    beq :+
    jsr BinToDec_8bit
    .repeat 3, i
        lda bcdOutput+i
        sta Combo_TilesP2+i
    .endrepeat
    jmp :++
:
    lda #' '
    sta Combo_TilesP2+0
    sta Combo_TilesP2+1
    lda #'0'
    sta Combo_TilesP2+2
:

    lda Lines+0
    sta bcdInput+0
    lda Lines+1
    sta bcdInput+1
    jsr BinToDec_Shift

    lda bcdOutput+3
    cmp #' '
    bne :+ ; max out at 9999
    .repeat 4, i
        lda bcdOutput+4+i
        sta Line_TilesP1+i
    .endrepeat
    jmp :++
:
    lda #'9'
    .repeat 4, i
        sta Line_TilesP1+i
    .endrepeat
:

    lda Lines+2
    sta bcdInput+0
    lda Lines+3
    sta bcdInput+1
    jsr BinToDec_Shift

    lda bcdOutput+3
    cmp #' '
    bne :+ ; max out at 9999
    .repeat 4, i
        lda bcdOutput+4+i
        sta Line_TilesP2+i
    .endrepeat
    jmp :++
:
    lda #'9'
    .repeat 4, i
        sta Line_TilesP2+i
    .endrepeat
:

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

    lda #$21
    sta $2006
    lda #$EF
    sta $2006
    .repeat 3, i
        lda Combo_TilesP1+i
        sta $2007
    .endrepeat

    lda #$22
    sta $2006
    lda #$CF
    sta $2006
    .repeat 3, i
        lda Combo_TilesP2+i
        sta $2007
    .endrepeat

    lda #$22
    sta $2006
    lda #$4E
    sta $2006
    .repeat 4, i
        lda Line_TilesP1+i
        sta $2007
    .endrepeat

    lda #$23
    sta $2006
    lda #$2E
    sta $2006
    .repeat 4, i
        lda Line_TilesP2+i
        sta $2007
    .endrepeat

    rts

VsHoldP1 = $2083+MMC5_OFFSET
VsNextP1 = $2089+MMC5_OFFSET
VsHoldP2 = $2093+MMC5_OFFSET
VsNextP2 = $2099+MMC5_OFFSET

IrqVsGame_Unrolled:

    ; clear all four spots
    lda #0
    .repeat 2, j
        .repeat 4, i
            sta VsHoldP1+i+(j*32)
            sta VsNextP1+i+(j*32)
            sta VsHoldP2+i+(j*32)
            sta VsNextP2+i+(j*32)
        .endrepeat
    .endrepeat

;
; P1 Hold
    ldx HoldPiece+0
    bmi @noP1Hold
    lda BlockBg_Tiles, x
    sta TmpA
    lda #32
    sta MMC5_MultB

    lda HoldPiece+0
    asl a
    asl a
    asl a
    asl a
    tax ; X has offset into BlockOffsets tables

    lda #4
    sta TmpX

:
    lda BlockOffsets_Y, x
    sta MMC5_MultA
    clc
    lda MMC5_MultA
    adc BlockOffsets_X, x
    tay
    lda TmpA
    sta VsHoldP1, y
    inx
    dec TmpX
    bne :-

@noP1Hold:
;
; P1 Next
    ldx BagP1+0
    lda BlockBg_Tiles, x
    sta TmpA
    lda #32
    sta MMC5_MultB

    lda BagP1+0
    asl a
    asl a
    asl a
    asl a
    tax ; X has offset into BlockOffsets tables

    lda #4
    sta TmpX

:
    lda BlockOffsets_Y, x
    sta MMC5_MultA
    clc
    lda MMC5_MultA
    adc BlockOffsets_X, x
    tay
    lda TmpA
    sta VsNextP1, y
    inx
    dec TmpX
    bne :-

;
; P2 Hold
    ldx HoldPiece+1
    bmi @noP2Hold
    lda BlockBg_Tiles, x
    sta TmpA
    lda #32
    sta MMC5_MultB

    lda HoldPiece+1
    asl a
    asl a
    asl a
    asl a
    tax ; X has offset into BlockOffsets tables

    lda #4
    sta TmpX

:
    lda BlockOffsets_Y, x
    sta MMC5_MultA
    clc
    lda MMC5_MultA
    adc BlockOffsets_X, x
    tay
    lda TmpA
    sta VsHoldP2, y
    inx
    dec TmpX
    bne :-

@noP2Hold:
;
; P2 Next
    ldx BagP2+0
    lda BlockBg_Tiles, x
    sta TmpA
    lda #32
    sta MMC5_MultB

    lda BagP2+0
    asl a
    asl a
    asl a
    asl a
    tax ; X has offset into BlockOffsets tables

    lda #4
    sta TmpX

:
    lda BlockOffsets_Y, x
    sta MMC5_MultA
    clc
    lda MMC5_MultA
    adc BlockOffsets_X, x
    tay
    lda TmpA
    sta VsNextP2, y
    inx
    dec TmpX
    bne :-

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

; FIXME: this is borked
CountDirtyClear:
    lda BlockY, y
    sec
    sbc #1
    sta TmpA

    tya
    pha

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
    lda #0
    sta TmpB
@count:
    lda (AddressPointer1), y
    beq @nope
    cmp #3
    bne :+
    lda #1
    sta TmpB
:
    iny
    dex
    bne @count
    jmp @next

@nope:
    sta TmpB

@next:
    lda TmpB
    beq :+
    dec DirtyLeft
:
    inc TmpY
    lda TmpY
    cmp #4
    bne @loop

    pla
    tay
    rts

UpdateActiveBlocks_Vs:
    tya ; Preserve Y
    pha

    .ifdef DEBUG_COLORS
        lda #%0101_1110
        sta $2001
    .endif
    ldy #Player1
    jsr CalculateGhost

    .ifdef DEBUG_COLORS
        lda #%1001_1110
        sta $2001
    .endif
    ldy #Player2
    jsr CalculateGhost
    .ifdef DEBUG_COLORS
        lda #%0001_1110
        sta $2001
    .endif

    ; P1 first
    ldx CurrentBlock+0
    lda GameState+0
    cmp #GS::Fall
    bne :+
    lda BlockColors, x
    sta Palettes+(0*4)+1+16
    lda BlockColors_Ghost, x
    sta Palettes+(0*4)+2+16
:

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

    lda GhostY+0
    asl a
    asl a
    asl a
    sta TmpZ

    .repeat 4, i
        lda BlockOffsets_Y, x
        tay
        lda VsBlockGridLocationY, y
        clc
        adc TmpY
        sta SpriteP1+0+(i*4)

        lda VsBlockGridLocationY, y
        clc
        adc TmpZ
        sta SpriteGhostP1+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda VsBlockGridLocationX, y
        clc
        adc TmpX
        sta SpriteP1+3+(i*4)
        sta SpriteGhostP1+3+(i*4)

        lda #0
        sta SpriteP1+2+(i*4)
        sta SpriteGhostP1+2+(i*4)

        lda #$11
        sta SpriteP1+1+(i*4)
        lda #$12
        sta SpriteGhostP1+1+(i*4)

        .if i < 3
            inx
        .endif
    .endrepeat

    lda BlockY+0
    cmp GhostY+0
    bne :+
    lda #$FF
    .repeat 4, i
        sta SpriteGhostP1+(i*4)
    .endrepeat
:

    lda GameState+0
    cmp #GS::Clear
    bne :+
    lda #$FF
    .repeat 4, i
        sta SpriteP1+(i*4)
    .endrepeat
:

    ; P2 second
    ldx CurrentBlock+1
    lda GameState+1
    cmp #GS::Fall
    bne :+
    lda BlockColors, x
    sta Palettes+(1*4)+1+16
    lda BlockColors_Ghost, x
    sta Palettes+(1*4)+2+16
:

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

    lda GhostY+1
    asl a
    asl a
    asl a
    sta TmpZ

    .repeat 4, i
        lda BlockOffsets_Y, x
        tay
        lda VsBlockGridLocationY, y
        clc
        adc TmpY
        sta SpriteP2+0+(i*4)

        lda VsBlockGridLocationY, y
        clc
        adc TmpZ
        sta SpriteGhostP2+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda VsBlockGridLocationX, y
        clc
        adc TmpX
        sta SpriteP2+3+(i*4)
        sta SpriteGhostP2+3+(i*4)

        lda #1
        sta SpriteP2+2+(i*4)
        sta SpriteGhostP2+2+(i*4)

        lda #$11
        sta SpriteP2+1+(i*4)
        lda #$12
        sta SpriteGhostP2+1+(i*4)

        .if i < 3
            inx
        .endif
    .endrepeat

    lda BlockY+1
    cmp GhostY+1
    bne :+
    lda #$FF
    .repeat 4, i
        sta SpriteGhostP2+(i*4)
    .endrepeat
:

    lda GameState+1
    cmp #GS::Clear
    bne :+
    lda #$FF
    .repeat 4, i
        sta SpriteP2+(i*4)
    .endrepeat
:

    pla
    tay
    rts

; Losing player in Y
VsModeGameOver:
    lda #%0001_1110
    sta $2001
    jsr ClearSprites

VsModeGameOver_Frame:
    jsr WaitForIRQ
    jmp VsModeGameOver_Frame

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

VsBlockGridLocationY:
    .repeat 21, i
        .byte VsBlockLocation_Y+(i*8)
    .endrepeat

VsBlockGridLocationX:
    .repeat 11, i
        .byte VsBlockLocation_X+(i*8)
    .endrepeat

