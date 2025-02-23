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
    ;.byte 1, 1, 0
    .byte 255, 1, 1
    ;.byte 255, 1, 1
    .byte 1, 255, 0
    ;.byte 1, 255, 0
    .byte 255, 255, 1
    ;.byte 255, 255, 1

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
    .byte $20, $15, $27, $20

StartGame:
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

    ; TODO: this
    ;jsr InitDirtyBoard
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

    jsr InitBags
    jsr NextBlock

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
    lda #SOFT_SPEED
    sta Speed_Soft

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

    lda Option_EnableHardDrop
    beq :+
    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    jsr DoHardDrop
    jmp @hardDropDone
:

    lda Option_EnableHold
    beq :+
    lda #BUTTON_SELECT ; select
    jsr ButtonReleased
    beq :+
    ldy #Player1
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

    lda #BUTTON_LEFT
    jsr ButtonReleased
    beq :+
    lda #$FF
    sta RepeatLeft
:

    lda RepeatLeft
    bmi :+
    dec RepeatLeft
    bpl :+
    lda Option_ShiftRepeat
    sta RepeatLeft
    jmp @buttonLeft
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :++
    lda Option_ShiftStart
    sta RepeatLeft
@buttonLeft:
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
    jsr ButtonReleased
    beq :+
    lda #$FF
    sta RepeatRight
:

    lda RepeatRight
    bmi :+
    dec RepeatRight
    bpl :+
    lda Option_ShiftRepeat
    sta RepeatRight
    jmp @buttonRight
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :++
    lda Option_ShiftStart
    sta RepeatRight
@buttonRight:
    inc BlockX
    lda BlockX
    cmp #BoardWidth+1
    bcc :+
    lda #BoardWidth
    sta BlockX
:
    jsr CheckCollide_Rotate
    beq :+
    dec BlockX
:

    lda SoftDrop
    beq @noSoft
    sec
    lda DropSpeed
    sbc Speed_Soft
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
    lda Speed_Drop
    sta DropSpeed
    jsr CheckFallCollision
@noDrop:

@hardDropDone:
    jsr CalcScore

    lda BlockX
    sta CurrentX
    lda BlockY
    sta CurrentY

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

    jsr WaitForIRQ
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

DoHardDrop:
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

    lda GhostY
    sta BlockY
    sta CurrentY

    ldy #Player1
    jsr PlaceBlock
    jsr CheckRowClear

    ldx #.sizeof(ClearRows)-1
:   lda ClearRows, x
    bne @StartClearRows
    dex
    bpl :-

    jsr NextBlock
    rts

@StartClearRows:
    jmp StartClearRows

ClearCountScores:
    .byte 0, 1, 3, 5, 8

CalcScore:
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
    sta BagA, x
    sta BagB, x
    inx
    cpx #7
    bne :-
    rts

InitBags:
    lda CurrentGameMode+GameMode::BaseType
    cmp #GameBaseType::SingleBlock
    bne :+
    jmp InitBag_SingleBlock
:

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

; Doesn't do any rotational collision stuff, just block
; gravity.
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

    ;jsr CheckCollide_WithGrid
    ldy #0
    jsr CheckCollide_Grid
    bne @CollideBottom
    rts

@CollideBottom:
    dec BlockY
    ldy #Player1
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

    ldy #0
    .repeat 10
        lda (AddressPointer1), y
        beq @next
        iny
    .endrepeat
    ldy BlockY
    lda #1
    sta ClearRows, y
    inc ClearCount

@next:
    dec TmpX
    bne @loop

    lda ClearCount
    sta UpdateCombo
    bne :+
    lda #$FF
    sta Combo
    rts
:

    clc
    inc Combo
    rts

DoClearRow:
    ldy #0
    .repeat 10
    lda (AddressPointer2), y
    sta (AddressPointer1), y
    iny
    .endrepeat
    rts

CheckCollide_Rotate:
    ldy #0
    jsr CheckCollide_Walls
    beq :+
    rts
:
    ldy #0
    jmp CheckCollide_Grid

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

    jmp LoadBlock

NextBlock:
    lda #$00
    sta HeldSwapped

NextBlock_Swap:
    lda Speed_Drop
    sta DropSpeed

    lda CurrentGameMode+GameMode::BaseType
    cmp #GameBaseType::SingleBlock
    bne :+
    lda CurrentGameMode+GameMode::TypeArg
    sta CurrentBlock
    jmp @afterBag
:

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

@afterBag:
    .ifdef DEBUG_PIECE
    lda #DEBUG_PIECE
    sta CurrentBlock
    .endif

    lda #BLOCK_START_X
    sta BlockX

    ldy CurrentBlock
    lda BlockStart_Y, y
    sta BlockY
    sta CurrentY

    lda #0
    sta BlockRotation
    sta SoftDrop

    lda #$FF
    sta RepeatRight
    sta RepeatLeft


    jsr LoadBlock
    ;jsr CheckCollide_WithGrid
    ldy #0
    jsr CheckCollide_Grid
    beq :+
    jmp DedTransition
:
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

UpdateBlock:

    lda #%0101_1110
    sta $2001
    jsr CalculateGhost
    lda #%0001_1110
    sta $2001

;
; Update both block and ghost sprites
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

    lda GhostY
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

        ;lda TmpA
        lda #0
        sta SpriteP1+2+(i*4)
        sta SpriteGhostP1+2+(i*4)

        ;lda TmpB
        lda #$11
        sta SpriteP1+1+(i*4)
        lda #$12
        sta SpriteGhostP1+1+(i*4)

        .if i < 3
            inx
        .endif
    .endrepeat

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
