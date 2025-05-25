.include "game.inc"

BlockLocation_X = 88 - 8
BlockLocation_Y = 40 - 1

GAMEOVER_START_X = 104
GAMEOVER_START_Y = 109

SCORE_ADDR = $2202
LINES_ADDR = $2262
COMBO_ADDR = $21A6
LEVEL_ADDR = $22C4

HOLD_ADDR  = $2139 + MMC5_OFFSET
NEXT_ADDR_START = $21D9 + MMC5_OFFSET

; Lines per level
LEVEL_LENGTH = 10

ShakeTable:
    ; X, Y, Nametable
    .byte 1, 1, 0
    .byte 255, 1, 1
    .byte 1, 255, 0
    .byte 255, 255, 1

ShakeTable_Length = (* - ShakeTable) / 3

DropSpeeds:
    .byte 60
    .byte 55
    .byte 50
    .byte 45
    .byte 40
    .byte 35
    .byte 30
    .byte 25
    .byte 20
    .byte 15
    .byte 10
    .byte 8
    .byte 6
    .byte 4
    .byte 3
    .byte 2
    .byte 1
    .byte 0 ; one frame per cell
SPEED_LENGTH = * - DropSpeeds

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

    ldx DropShake
    bmi @noShake

    stx MMC5_MultA
    lda #3
    sta MMC5_MultB
    ldx MMC5_MultA

    lda ShakeTable+0, x
    sta ScrollX
    lda ShakeTable+1, x
    sta ScrollY
    lda ShakeTable+2, x
    ora #$80
    sta PpuControl

    dec DropShake
    rts

@noShake:
    lda #0
    sta ScrollX
    sta ScrollY
    lda #%1000_0000
    sta PpuControl
    rts

GameOverPalette:
    .byte BG_COLOR, $15, $27, $20

DebugSpritePal:
    .byte BG_COLOR, $01, $07, $10

StartGame:
    lda #$FF
    sta CurrentBlock+1

    lda ModeSelection
    cmp #MMSel::Marathon
    beq @std

    cmp #MMSel::Classic
    beq @classic

    cmp #MMSel::NoHold
    beq @hold

    cmp #MMSel::DirtyBoard
    beq @dirty

    cmp #MMSel::SingleBlock
    beq @single

    cmp #MMSel::TimeAttack
    beq @time

    ; default to standard
@std:
    lda #GameBaseType::Standard
    sta CurrentGameMode+GameMode::BaseType
    lda #GameStandardArgs::Standard
    sta CurrentGameMode+GameMode::TypeArg
    lda #0
    sta CurrentGameMode+GameMode::HsIndex
    jmp InitGame

@classic:
    lda #GameBaseType::Standard
    sta CurrentGameMode+GameMode::BaseType
    lda #GameStandardArgs::Classic
    sta CurrentGameMode+GameMode::TypeArg
    lda #11
    sta CurrentGameMode+GameMode::HsIndex
    jmp InitGame

@hold:
    lda #GameBaseType::Standard
    sta CurrentGameMode+GameMode::BaseType
    lda #GameStandardArgs::NoHold
    sta CurrentGameMode+GameMode::TypeArg
    lda #10
    sta CurrentGameMode+GameMode::HsIndex
    jmp InitGame

@dirty:
    lda #GameBaseType::Standard
    sta CurrentGameMode+GameMode::BaseType
    lda #GameStandardArgs::DirtyBoard
    sta CurrentGameMode+GameMode::TypeArg
    lda #9
    sta CurrentGameMode+GameMode::HsIndex

    jmp InitGame

@single:
    lda #GameBaseType::SingleBlock
    sta CurrentGameMode+GameMode::BaseType
    lda SingleBlockId
    sta CurrentGameMode+GameMode::TypeArg
    clc
    adc #1
    sta CurrentGameMode+GameMode::HsIndex
    jmp InitGame

@time:
    lda #GameBaseType::TimeAttack
    sta CurrentGameMode+GameMode::BaseType
    lda TimeModeType
    sta CurrentGameMode+GameMode::TypeArg
    clc
    adc #6
    sta CurrentGameMode+GameMode::HsIndex

InitGame:
    lda #.lobyte(GamePalettes)
    sta AddressPointer1+0
    lda #.hibyte(GamePalettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes
    jsr LoadSpPalettes

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
    iny
    cpy #200
    bne :-

    lda CurrentGameMode+GameMode::BaseType
    cmp #GameBaseType::Standard
    bne :+
    lda CurrentGameMode+GameMode::TypeArg
    cmp #GameStandardArgs::DirtyBoard
    bne :+
    jsr InitDirtyBoard
:

    lda CurrentGameMode+GameMode::BaseType
    cmp #GameBaseType::SingleBlock
    bne :+
    jsr InitBag_SingleBlock
    jmp :++
:
    ldy #Player1
    jsr ShuffleBag_Init
:
    ldy #Player1
    jsr NextBlock

    lda #$FF
    sta CurrentBlock+1

    lda #0
    ldx #0
:
    sta Level, x
    inx
    cpx #.sizeof(Level)
    bne :-

    ldx #ScoreEntry::Time
:
    sta CurrentScore, x
    inx
    cpx #.sizeof(CurrentScore)
    bne :-

    lda #'0'
    ldx #0
:
    sta HighScore, x
    inx
    cpx #.sizeof(HighScore)
    bne :-

    lda #$FF
    sta Combo
    sta DropShake
    sta RepeatLeft
    sta RepeatRight

    lda #' '
    sta Combo_Tiles+0
    lda #'0'
    sta Combo_Tiles+1

    lda #LEVEL_LENGTH-1
    sta LinesToNextLevel

    lda #0
    sta TimeFrame

    ;lda #SPEED
    lda DropSpeeds+0
    sta DropSpeed
    sta Speed_Drop

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
    sta PpuControl
    sta $2000

    lda #$00
    sta $2003

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

    lda #$00
    sta ScrollX
    sta ScrollY
    sta $2005
    sta $2005

    lda #.lobyte(NmiGame)
    sta NmiHandler+0
    lda #.hibyte(NmiGame)
    sta NmiHandler+1

    jsr WaitForNMI
    jsr WaitForNMI

    lda #$FF
    sta HoldPiece

FrameGame:
    SetIRQ 2, IrqDrawBoard

    jsr ReadControllers

    ldy #Player1
    jsr DoPlayer

    ldy #Player1
    jsr UpdateBlock

    ldx TimeFrame
    inx
    cpx #60
    bne :+
    inc CurrentScore+ScoreEntry::Time+0
    ldx #0
:
    stx TimeFrame

    ldx CurrentScore+ScoreEntry::Time+0
    cpx #60
    bne :+
    inc CurrentScore+ScoreEntry::Time+1
    ldx #0
:
    stx CurrentScore+ScoreEntry::Time+0

    jsr CalcScore

    ldx DropShake
    bmi @noShake

    stx MMC5_MultA
    lda #3
    sta MMC5_MultB
    ldx MMC5_MultA

    ;sta ScrollX
    ;sta ScrollY

    .repeat 4, i
        sec
        lda SpriteP1+3+(i*4)
        sbc ShakeTable+0, x
        sta SpriteP1+3+(i*4)

        sec
        lda SpriteP1+0+(i*4)
        sbc ShakeTable+1, x
        sta SpriteP1+0+(i*4)
    .endrepeat

@noShake:

    lda TSpin+0
    and #$80
    beq :+
    lda #'R'
    jmp :++
:
    lda #' '
:   sta TSpinDebugSprite+1

    lda TSpin+0
    and #$7F
    beq :+
    lda #'T'
    sta TSpinDebugSprite+1
:
    lda #40
    sta TSpinDebugSprite+0
    lda #24
    sta TSpinDebugSprite+3
    lda #0
    sta TSpinDebugSprite+2

    jsr WaitForIRQ
    jmp FrameGame

InitDirtyBoard:
    ldy #10*10
@loop:
    inc rng_index
    ldx rng_index
    lda PieceRng, x
    cmp #4
    bcc :+
    lda #3
    sta FieldGrid, y
:
    iny
    cpy #200
    bne @loop

    lda #10
    sta MMC5_MultB
    ldy #0
    sty TmpY
    sty DirtyLeft
@count:
    jsr @isRowClear
    inc TmpY
    lda TmpY
    cmp #20
    bne @count
    rts

@isRowClear:
    ldx #10
    lda TmpY
    sta MMC5_MultA
    ldy MMC5_MultA
@ctop:
    lda FieldGrid, y
    beq @next
    inc DirtyLeft
    rts

@next:
    iny
    dex
    bpl @ctop
    rts

ClearCountScores:
    .byte 0, 1, 3, 5, 8

CalcScore:
    lda GameState+0
    cmp #GS::Garbage
    bne :+
    lda GameStateArg+0
    bne :+
    lda TSpin+0
    and #$7F
    beq :+
    clc
    lda #100
    adc CurrentScore+ScoreEntry::Score+0
    sta CurrentScore+ScoreEntry::Score+0
:

    clc
    lda DropScore
    adc CurrentScore+ScoreEntry::Score+0
    sta CurrentScore+ScoreEntry::Score+0

    lda CurrentScore+ScoreEntry::Score+1
    adc #0
    sta CurrentScore+ScoreEntry::Score+1

    lda CurrentScore+ScoreEntry::Score+2
    adc #0
    sta CurrentScore+ScoreEntry::Score+2

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
    lda CurrentScore+ScoreEntry::Score+0
    adc TmpM+0
    sta CurrentScore+ScoreEntry::Score+0

    lda CurrentScore+ScoreEntry::Score+1
    adc TmpM+1
    sta CurrentScore+ScoreEntry::Score+1

    lda CurrentScore+ScoreEntry::Score+2
    adc TmpM+2
    sta CurrentScore+ScoreEntry::Score+2

@done:

    ; Level calculation stuff
    lda ClearCount
    beq @levelMathDone
    sec
    lda LinesToNextLevel
    sbc ClearCount
    sta LinesToNextLevel
    bpl @levelMathDone
    clc
    adc #LEVEL_LENGTH
    sta LinesToNextLevel
    lda #1
    sta NextDropSpeed
    inc Level

    ldx Level
    dex
    cpx #SPEED_LENGTH
    bcc :+
    ldx #SPEED_LENGTH-1
:

    lda DropSpeeds, x
    sta Speed_Drop
    sta DropSpeed


@levelMathDone:

    clc
    lda CurrentScore+ScoreEntry::Lines
    adc ClearCount
    sta CurrentScore+ScoreEntry::Lines

    lda UpdateCombo
    beq @noCombo

    ; TODO: add combo to score

    ;lda #50
    ;sta MMC5_MultA
    ;lda Combo+0
    ;sta MMC5_MultB

    ;lda MMC5_MultA
    ;sta MathA+0

    ;lda #50
    ;sta MMC5_MultA
    ;lda Combo+1
    ;sta MMC5_MultB

    ;lda MMC5_MultA
    ;sta MathA+1

    ;lda MathA+0
    ;sta MMC5_MultA
    ;lda 

@noCombo:

    ; Is combo -1? draw 0 if so
    lda Combo
    cmp #$FF
    bne :+

    lda #' '
    sta Combo_Tiles+0
    lda #'0'
    sta Combo_Tiles+1
    jmp @comboDone
:
    ldy Combo
    lda ComboTiles_A, y
    sta Combo_Tiles+0
    lda ComboTiles_B, y
    sta Combo_Tiles+1

@comboDone:

    .repeat .sizeof(ScoreEntry::Score), i
    lda CurrentScore+ScoreEntry::Score+i
    ;sta TmpScore+i
    ;sta Bin_Input+i
    sta bcdInput+i
    .endrepeat

    ; 14,117 cycles
    ;jsr BinToDec
    ; ~3,000 cycles (!!)
    jsr BinToDec_Shift

    .repeat .sizeof(bcdOutput), i
    lda bcdOutput+i
    sta Score_Tiles+i
    .endrepeat

    .repeat 7, i
        lda Score_Tiles+i
        cmp #$30
        bne @scoreTiles_done
        lda #' '
        sta Score_Tiles+i
    .endrepeat
@scoreTiles_done:

    .repeat .sizeof(ScoreEntry::Lines), i
    lda CurrentScore+ScoreEntry::Lines+i
    ;sta Bin_Input+i
    sta bcdInput+i
    .endrepeat

    jsr BinToDec_Shift

    .repeat .sizeof(Lines_Tiles), i
    ;lda Bin_Tiles+i
    lda bcdOutput+i+2
    sta Lines_Tiles+i
    .endrepeat

    .repeat 5, i
        lda Lines_Tiles+i
        cmp #$30
        bne @linesTiles_done
        lda #' '
        sta Lines_Tiles+i
    .endrepeat
@linesTiles_done:

    lda Level+0
    sta Bin_Input+0
    lda Level+1
    sta Bin_Input+1
    lda #0
    sta Bin_Input+2

    jsr BinToDec_Sm

    .repeat .sizeof(Level_Tiles), i
    lda Bin_Tiles+2+i
    sta Level_Tiles+i
    .endrepeat

    lda #0
    sta ClearCount
    sta DropScore
    rts

DedNmi:
    jsr BareNmiHandler

    ldx DropShake
    bmi @noShake

    stx MMC5_MultA
    lda #3
    sta MMC5_MultB
    ldx MMC5_MultA

    lda ShakeTable+0, x
    sta ScrollX
    lda ShakeTable+1, x
    sta ScrollY
    lda ShakeTable+2, x
    ora #$80
    sta PpuControl

    dec DropShake
    rts

@noShake:
    lda #0
    sta ScrollX
    sta ScrollY
    lda #%1000_0000
    sta PpuControl
    rts

DedTransition:
    jsr UpdateBlock
    jsr WaitForIRQ
    DisableIRQ

    lda #$FF
    .repeat 4, i
        sta SpriteGhostP1+(i*4)
    .endrepeat

    lda #.lobyte(DedNmi)
    sta NmiHandler+0
    lda #.hibyte(DedNmi)
    sta NmiHandler+1

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

    jsr CheckForNewHighScore
    jsr WaitForNMI

DedFrame:

    jsr ReadControllers
    lda #BUTTON_START
    jsr ButtonPressed
    beq :+
    jmp @yup

:
    lda #BUTTON_A
    jsr ButtonPressed
    beq @nope
@yup:
    jsr WaitForNMI

    lda NewHsIndex
    bmi @toMenu
    lda #InitIndex::NewScore
    jmp GotoInit

@nope:

    jsr WaitForNMI
    jmp DedFrame

@toMenu:
    lda #InitIndex::Menu
    jmp GotoInit

InitBag_SingleBlock:
    lda #7
    sta BagLeft
    lda CurrentGameMode+GameMode::TypeArg
    ldx #0
:
    sta BagP1, x
    sta BagP1+7, x
    inx
    cpx #7
    bne :-
    rts

UpdateBlock:
    .ifdef DEBUG_COLORS
        lda #%0101_1110
        sta $2001
    .endif
    ldy #Player1
    jsr CalculateGhost
    .ifdef DEBUG_COLORS
        lda #%0001_1110
        sta $2001
    .endif

;
; Update both block and ghost sprites
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
        lda BlockGridLocationY, y
        clc
        adc TmpY
        sta SpriteP1+0+(i*4)

        lda BlockGridLocationY, y
        clc
        adc TmpZ
        sta SpriteGhostP1+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda BlockGridLocationX, y
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

@afterBlocks:

    ldx CurrentBlock+0
    lda GameState+0
    cmp #GS::Fall
    bne :+
    lda BlockColors, x
    sta Palettes+(0*4)+1+16
    lda BlockColors_Ghost, x
    sta Palettes+(0*4)+2+16
:

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
    beq :+
    cmp #GS::Garbage
    beq :+
    jmp :++
:
    lda #$FF
    .repeat 4, i
        sta SpriteP1+(i*4)
        sta SpriteGhostP1+(i*4)
    .endrepeat
:
    rts

IrqDrawBoard:

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
    .if j = 1
        lda Option_ShowNext
        bne :+
        jmp @noBag
:
    .endif

        lda BagP1+j
        asl a
        tax
        lda BlockTiles+0, x
        sta AddressPointer1+0
        lda BlockTiles+1, x
        sta AddressPointer1+1

        lda BagP1+j
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

@noBag:
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

InitDebug:
    lda #.lobyte(GamePalettes)
    sta AddressPointer1+0
    lda #.hibyte(GamePalettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes
    jsr LoadSpPalettes

    lda #.lobyte(GameOverPalette)
    sta AddressPointer1+0
    lda #.hibyte(GameOverPalette)
    sta AddressPointer1+1
    ldx #7
    jsr LoadPalette

    lda #.lobyte(DebugSpritePal)
    sta AddressPointer1+0
    lda #.hibyte(DebugSpritePal)
    sta AddressPointer1+1
    ldx #5
    jsr LoadPalette

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

    ; disable P2 stuff?
    lda #$FF
    sta CurrentBlock+1
    sta DropShake

    ; Clear P1 stuff
    lda #0
    sta CurrentBlock+0
    sta BlockRotation+0

    lda #Rot::CW ; Rot::CW
    sta RotationDirection

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

    lda #%0000_0001
    sta $5104

    lda #5
    sta BlockX
    sta BlockY

    lda #%1000_0000
    sta PpuControl
    sta $2000

    lda #$00
    sta $2003

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

    lda #$00
    sta ScrollX
    sta ScrollY
    sta $2005
    sta $2005

    lda #.lobyte(BareNmiHandler)
    sta NmiHandler+0
    lda #.hibyte(BareNmiHandler)
    sta NmiHandler+1

    jsr WaitForNMI

FrameDebug:
    SetIRQ 2, IrqDrawBoard
    jsr ReadControllers

    ldy #Player1

    lda #0
    sta TmpXX
    sta TmpYY

    ; decrement the kick lookup index with DOWN
    lda #BUTTON_DOWN
    jsr ButtonPressed
    beq :+
    dec dbgStep
    bpl :+
    lda #4
    sta dbgStep
:

    ; increment the kick lookup index with UP
    lda #BUTTON_UP
    jsr ButtonPressed
    beq :+
    inc dbgStep
    lda dbgStep
    cmp #5
    bcc :+
    lda #0
    sta dbgStep
:

    ; Rotation stuff
    lda #BUTTON_LEFT
    jsr ButtonPressed
    beq :+

    lda #Rot::CCW ; Rot::CCW
    sta RotationDirection

    dec BlockRotation+0
    bpl :+
    lda #3
    sta BlockRotation+0
:

    lda #BUTTON_RIGHT
    jsr ButtonPressed
    beq :+

    lda #Rot::CW ; Rot::CW
    sta RotationDirection

    inc BlockRotation+0
    lda BlockRotation+0
    cmp #4
    bcc :+
    lda #0
    sta BlockRotation+0
:

    ; Cycle through blocks
    lda #BUTTON_START
    jsr ButtonPressed
    beq :+
    inc CurrentBlock+0
    lda CurrentBlock+0
    cmp #7
    bcc :+
    lda #0
    sta CurrentBlock+0
:

    ; Rotation stuff
    lda #BUTTON_A
    jsr ButtonPressed
    beq :+

    lda #Rot::CW
    sta RotationDirection

    lda BlockRotation+0
    clc
    adc #1
    and #$03
    sta DebugRotation
    lda BlockRotation+0
    ;asl a
    ;ora #1
    ;sta dbgEnum
:

    lda #BUTTON_B
    jsr ButtonPressed
    beq :+

    lda #Rot::CCW
    sta RotationDirection

    lda BlockRotation+0
    sec
    sbc #1
    and #$03
    sta DebugRotation
    ;lda BlockRotation+0
    ;asl a
    ;sta dbgEnum
:

    jsr DebugKicks
    jsr DebugUpdateSprites

    jsr WaitForIRQ
    jmp FrameDebug

DebugUpdateSprites:
    lda CurrentBlock+0
    asl a
    asl a
    clc
    adc BlockRotation+0
    asl a
    asl a
    sta dbgOffsetActive

    lda CurrentBlock+0
    asl a
    asl a
    clc
    adc DebugRotation+0
    asl a
    asl a
    sta dbgOffsetGhost

    lda BlockX+0
    asl a
    asl a
    asl a
    sta dbgActive_X

    lda BlockY+0
    asl a
    asl a
    asl a
    sta dbgActive_Y

    lda GhostY+0
    asl a
    asl a
    asl a
    sta dbgGhost_Y

    lda GhostX+0
    asl a
    asl a
    asl a
    sta dbgGhost_X

    ldx dbgOffsetActive
    .repeat 4, i
        lda BlockOffsets_Y, x
        tay
        lda BlockGridLocationY, y
        clc
        adc dbgActive_Y
        sta SpriteP2+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda BlockGridLocationX, y
        clc
        adc dbgActive_X
        sta SpriteP2+3+(i*4)

        lda #0
        sta SpriteP2+2+(i*4)
        lda #$11
        sta SpriteP2+1+(i*4)
        .if i < 3
            inx
        .endif
    .endrepeat

    ldx dbgOffsetGhost
    .repeat 4, i
        lda BlockOffsets_Y, x
        tay
        lda BlockGridLocationY, y
        clc
        adc dbgGhost_Y
        sta SpriteGhostP1+0+(i*4)

        lda BlockOffsets_X, x
        tay
        lda BlockGridLocationX, y
        clc
        adc dbgGhost_X
        sta SpriteGhostP1+3+(i*4)

        lda #1
        sta SpriteGhostP1+2+(i*4)
        lda #$85
        sta SpriteGhostP1+1+(i*4)

        .if i < 3
            inx
        .endif
    .endrepeat

    ldx CurrentBlock+0
    lda BlockColors, x
    sta Palettes+(0*4)+1+16
    lda BlockColors_Ghost, x
    sta Palettes+(0*4)+2+16

    ; Debug index output
    lda dbgStep
    ora #$30
    sta GameOverSprites+(0*4)+1
    lda #0
    sta GameOverSprites+(0*4)+2
    lda #208
    sta GameOverSprites+(0*4)+3
    lda #16
    sta GameOverSprites+(0*4)+0

    lda BlockRotation+0
    ora #$30
    sta GameOverSprites+(1*4)+1
    lda #0
    sta GameOverSprites+(1*4)+2
    lda #208
    sta GameOverSprites+(1*4)+3
    lda #16+8
    sta GameOverSprites+(1*4)+0

    lda RotationDirection
    bne :+
    lda #' '
    jmp @rotDone
:   cmp #Rot::CCW ; Rot::CCW
    bne :+
    lda #'L'
    jmp @rotDone
:   lda #'R'

@rotDone:
    sta GameOverSprites+(2*4)+1
    lda #0
    sta GameOverSprites+(2*4)+2
    lda #208
    sta GameOverSprites+(2*4)+3
    lda #16+16
    sta GameOverSprites+(2*4)+0

    ; Active Origin
    lda #$8B
    sta SpriteP1+(0*4)+1
    lda #1
    sta SpriteP1+(0*4)+2

    ldx BlockY
    lda BlockGridLocationY, x
    sta SpriteP1+(0*4)+0
    ldx BlockX
    lda BlockGridLocationX, x
    sta SpriteP1+(0*4)+3

    ; Ghost Origin
    lda #$8C
    sta SpriteP1+(1*4)+1
    lda #1
    sta SpriteP1+(1*4)+2

    ldx GhostY
    lda BlockGridLocationY, x
    sta SpriteP1+(1*4)+0
    ldx GhostX
    lda BlockGridLocationX, x
    sta SpriteP1+(1*4)+3
    rts

DebugKicks:

    ; Clear field every frame
    lda #0
    ldx #0
:
    sta FieldGrid, x
    inx
    cpx #200
    bne :-

    lda #0
    sta TmpX ; rotation delta
    ldx RotationDirection

    lda CurrentBlock+0
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

    lda #1
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

    lda #255
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

    lda #1
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

    lda #255
    sta TmpX
@addrDone:

    ; Get offsets into both tables (non-wall and wall)
    clc
    lda BlockRotation
    adc TmpX
    and #$03 ; A is now index to row
    sta DebugRotation

    lda BlockRotation
    sta MMC5_MultA

    lda #5
    sta MMC5_MultB

    lda MMC5_MultA
    sta dbgKickTable      ; table is one byte entries
    asl a
    sta dbgKickTable_Wall ; table is two byte entries

    lda BlockX+0
    sta dbgBlockX

    lda BlockY+0
    sta dbgBlockY

    jsr AlignBlockWithField
    stx dbgBlockOffset ; offset into BlockOffsets_X/Y

    ; Offset into player field
    lda TmpY
    sta dbgFieldOffset

    ; AddressPointer1 - FieldGrid
    ; AddressPointer2 - KickOffsets
    ; AddressPointer3 - KickOffsets_Wall
    nop

    lda dbgKickTable
    clc
    adc dbgStep
    tay

    clc
    lda dbgFieldOffset
    adc (AddressPointer2), y
    sta TmpY ; offset into field, after kick

    clc
    lda dbgStep
    asl a
    clc
    adc dbgKickTable_Wall
    tay

    clc
    lda dbgBlockX
    adc (AddressPointer3), y
    sta dbgGhost_X

    iny
    clc
    lda dbgBlockY
    adc (AddressPointer3), y
    sta dbgGhost_Y

    ; Write ghost to field
    ldy #Player1

    lda CurrentBlock, y
    asl a
    asl a
    clc
    adc DebugRotation, y
    asl a
    asl a
    tax ; offset in BlockOffsets_X/Y

    ;ldx CurrentBlock, y
    ;lda BlockBg_Tiles, x
    lda #$13
    sta TmpA

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

    ldy #Player1
    lda dbgGhost_X
    sta GhostX+0
    lda dbgGhost_Y
    sta GhostY+0
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
    .repeat 21, i
        .byte BlockLocation_Y+(i*8)
    .endrepeat

BlockGridLocationX:
    .repeat 11, i
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
        .word NEXT_ADDR_START+(i*32)+(32*3*0)
    .endrepeat

BagRows1:
    .repeat 2, i
        .word NEXT_ADDR_START+(i*32)+(32*3*1)
    .endrepeat

BagRows2:
    .repeat 2, i
        .word NEXT_ADDR_START+(i*32)+(32*3*2)
    .endrepeat

BagRows3:
    .repeat 2, i
        .word NEXT_ADDR_START+(i*32)+(32*3*3)
    .endrepeat

; Tens
ComboTiles_A:
    .repeat 10
        .byte ' '
    .endrepeat

    .repeat 10
        .byte '1'
    .endrepeat

    .repeat 10
        .byte '2'
    .endrepeat

    .byte '3'
    .byte '3'

; Ones
ComboTiles_B:
    .repeat 10, i
        .byte '0' + i
    .endrepeat
    .repeat 10, i
        .byte '0' + i
    .endrepeat
    .repeat 10, i
        .byte '0' + i
    .endrepeat
    .byte '0'
    .byte '1'
