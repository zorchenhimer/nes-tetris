HS_SAVE_COUNT = 10
HS_LIST_SIZE = .sizeof(ScoreEntry) * HS_SAVE_COUNT

.struct ScoreDisplay
    Name  .byte 16
    Time  .byte 8
    Lines .byte 8
    Score .byte 8 ; only ValueA
.endstruct

HS_DISPLAY_SIZE = .sizeof(ScoreDisplay) * HS_SAVE_COUNT

HS_TITLE_ADDR = $2066
HS_TITLE_LEN  = 19
HS_NAME_START = $20C4

.enum EnTbl
LtrA
LtrB
LtrC
LtrD
LtrE
LtrF
LtrG
LtrH
LtrI
LtrJ
LtrK
LtrL
LtrM
LtrN
LtrO
LtrP
LtrQ
LtrR
LtrS
LtrT
LtrU
LtrV
LtrW
LtrX
LtrY
LtrZ
One
Two
Three
Four
Five
Six
Seven
Eight
Nine
Zero
Dash
Equal
Erase
Space
End
Tilde
BracketLeft
BracketRight
Pipe
Semicolon
Quote
Comma
Period
Slash
.endenum

.enum EnDir
Up
Down
Left
Right
.endenum

.out .sprintf(".sizeof(ScoreEntry): %d", .sizeof(ScoreEntry))
.out .sprintf("HS_LIST_SIZE: %d", HS_LIST_SIZE)
.out .sprintf("HS_DISPLAY_SIZE: %d", HS_DISPLAY_SIZE)

.pushseg
.segment "ZEROPAGE"

EN_Shifted: .res 1
EN_Selection: .res 1
EN_Cursor: .res 1

Save_CurrentList: .res 1

ScoreAnim_Direction: .res 1 ; 0 = right; 1 = left; 256 = nothing
ScoreAnim_DirectionIrq: .res 1 ; 0 = right; 1 = left; 256 = nothing
ScoreAnim_Frame:     .res 1
ScoreAnim_Scroll:    .res 1
Score_DrawTitle:     .res 1

NewHsIndex: .res 1

.segment "SAVERAM"
Save_Standard:  .res HS_LIST_SIZE
Save_BlockZ:    .res HS_LIST_SIZE
Save_BlockS:    .res HS_LIST_SIZE
Save_BlockT:    .res HS_LIST_SIZE
Save_BlockL:    .res HS_LIST_SIZE
Save_BlockJ:    .res HS_LIST_SIZE
Save_TimeLines: .res HS_LIST_SIZE ; time based
Save_TimeScore: .res HS_LIST_SIZE
Save_Dirty:     .res HS_LIST_SIZE ; time based
Save_NoHold:    .res HS_LIST_SIZE
Save_Classic:   .res HS_LIST_SIZE

.out .sprintf("Save size: %d ($%04X)", (* - Save_Standard), (* - Save_Standard))

Save_CheckVal: .res 5

Option_GhostFlash:  .res 1 ; 1 enable flash, 0 disable flash
Option_ScreenShake: .res 1 ; 1 enable shake
Option_ShiftStart:  .res 1
Option_ShiftRepeat: .res 1
Option_ShowNext:    .res 1
Option_ShowCurrent: .res 1
Option_ShowGhost:   .res 1
Option_EnableHold:  .res 1
Option_EnableHardDrop: .res 1
Debug_ClearSave: .res 1

Scores_Screen: .res HS_DISPLAY_SIZE
Title_Screen: .res HS_TITLE_LEN

.segment "BSS"

;Scores_A: .res .sizeof(ScoreEntry)
;Scores_B: .res .sizeof(ScoreDisplay)

.segment "OAM"
EN_SelectSprites: .res 4*4
EN_CursorSprite: .res 4

.segment "PAGE_INIT"

Save_CheckVal_Check:
    .byte "Zorch"

Save_Palettes:
    .byte $0F, $20, $0F, $10
    .byte $0F, $22, $32, $0F
    .byte $0F, $25, $35, $0F
    .byte $0F, $2B, $3B, $0F

    ;.byte $0F, $21, $31, $0F

Scores_Scroll_Increment = 25
Scores_Scroll:
    .repeat 10, i
        .byte Scores_Scroll_Increment*(i+1)
    .endrepeat
Scores_Scroll_Len = * - Scores_Scroll

Scores_ScrollRev:
    .repeat 10, i
        .byte Scores_Scroll_Increment*(Scores_Scroll_Len-i-1)
    .endrepeat

SaveEntryOffsets:
    .repeat HS_SAVE_COUNT, i
    ;.out .sprintf("SaveEntryOffset: %d", (i*.sizeof(ScoreEntry)))
    .byte i*.sizeof(ScoreEntry)
    .endrepeat

SaveTypeList:
    .word Save_Standard
    .word Save_BlockZ
    .word Save_BlockS
    .word Save_BlockT
    .word Save_BlockL
    .word Save_BlockJ
    .word Save_TimeLines
    .word Save_TimeScore
    .word Save_Dirty
    .word Save_NoHold
    .word Save_Classic

HS_LIST_COUNT = (* - SaveTypeList) / 2

SaveTypeTitles:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word :+++++
    .word :++++++
    .word :+++++++
    .word :++++++++
    .word :+++++++++
    .word :++++++++++
    .word :+++++++++++

:   .asciiz "Standard"
:   .asciiz "Only Z Blocks"
:   .asciiz "Only S Blocks"
:   .asciiz "Only T Blocks"
:   .asciiz "Only L Blocks"
:   .asciiz "Only J Blocks"
:   .asciiz "Time Attack: Lines"
:   .asciiz "Time Attack: Score"
:   .asciiz "Dirty Board"
:   .asciiz "No Hold"
:   .asciiz "Classic"

SaveTypeColumn:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word :+++++
    .word :++++++
    .word :+++++++
    .word :++++++++
    .word :+++++++++
    .word :++++++++++
    .word :+++++++++++

:   .asciiz "Scores"
:   .asciiz "Scores"
:   .asciiz "Scores"
:   .asciiz "Scores"
:   .asciiz "Scores"
:   .asciiz "Scores"
:   .asciiz "Times"
:   .asciiz "Times"
:   .asciiz "Times"
:   .asciiz "Scores"
:   .asciiz "Scores"

.macro ClearSaveTable addr
    lda #.lobyte(addr)
    sta AddressPointer1+0
    lda #.hibyte(addr)
    sta AddressPointer1+1
    jsr Save_ResetTable
.endmacro

InitRam:

    ; Check for the magic values in ram.  If they
    ; aren't there, reset all of save ram.
    ldx #0
@loop:
    lda Save_CheckVal, x
    cmp Save_CheckVal_Check, x
    bne ClearRam
    inx
    cpx #5
    bne @loop
    rts

ClearRam:
    EnableRam

    lda #GHOST_FLASH
    sta Option_GhostFlash
    lda #SCREEN_SHAKE
    sta Option_ScreenShake
    lda #BUTTON_REPEAT_START
    sta Option_ShiftStart
    lda #BUTTON_REPEAT
    sta Option_ShiftRepeat
    lda #0
    sta Debug_ClearSave

    ClearSaveTable Save_Standard
    ClearSaveTable Save_BlockZ
    ClearSaveTable Save_BlockS
    ClearSaveTable Save_BlockT
    ClearSaveTable Save_BlockL
    ClearSaveTable Save_BlockJ
    ClearSaveTable Save_TimeLines
    ClearSaveTable Save_TimeScore
    ClearSaveTable Save_Dirty
    ClearSaveTable Save_NoHold
    ClearSaveTable Save_Classic

    ldy #5
    ldx #0
:   lda Save_CheckVal_Check, x
    sta Save_CheckVal, x
    inx
    dey
    bne :-

    DisableRam

    rts

; reset with dummy values
; Table in AddressPointer1
Save_ResetTable:
    ldy #0
    ldx #HS_LIST_SIZE
@outer:
    lda DummyData, y
    sta (AddressPointer1), y
    iny
    dex
    bne @outer
    rts

InitScores:
    EnableRam
    lda #.lobyte(Save_Palettes)
    sta AddressPointer1+0
    lda #.hibyte(Save_Palettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes

    lda #%0000_0010
    sta $5104

    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
:   lda Palettes, x
    sta $2007
    inx
    cpx #4*8
    bne :-

    ;
    ; Time colors
    ldx #0
    lda #%0100_0001
:
    .repeat 10, i
        sta MMC5_OFFSET+HS_NAME_START+32+(i*64), x
    .endrepeat

    inx
    cpx #8
    bne :-

    ; header
    ldx #0
    lda #%0100_0000
:
    sta MMC5_OFFSET+HS_NAME_START-32, x
    inx
    cpx #8
    bne :-

    ;
    ; Lines colors
    ldx #0
    lda #%1000_0001
:
    .repeat 10, i
        sta MMC5_OFFSET+HS_NAME_START+32+9+(i*64), x
    .endrepeat

    inx
    cpx #8
    bne :-

    ; header
    ldx #0
    lda #%1000_0000
:
    sta MMC5_OFFSET+HS_NAME_START-32+9, x
    inx
    cpx #8
    bne :-

    ;
    ; Scores colors
    ldx #0
    lda #%1100_0001
:
    .repeat 10, i
        sta MMC5_OFFSET+HS_NAME_START+32+18+(i*64), x
    .endrepeat

    inx
    cpx #8
    bne :-

    ; header
    ldx #0
    lda #%1100_0000
:
    sta MMC5_OFFSET+HS_NAME_START-32+18, x
    inx
    cpx #8
    bne :-

    lda #%0000_0001
    sta $5104

    lda CurrentGameMode+GameMode::HsIndex
    sta Save_CurrentList
    jsr LoadScores
    lda PpuControl
    ora #1
    sta PpuControl

    jsr DrawScores_VerticalCol1
    jsr DrawScores_VerticalCol2

    lda PpuControl
    and #%1111_1000
    ora #%0000_0001
    sta $2000
    sta PpuControl

    jsr DrawScores_VerticalCol3

    lda #.lobyte(NMI_Scores)
    sta NmiHandler+0
    lda #.hibyte(NMI_Scores)
    sta NmiHandler+1

    lda #%1000_0000
    sta PpuControl
    sta $2000

    lda #%0000_1110
    sta PpuMask

    lda #0
    sta ScrollX
    sta ScrollY

    jsr WaitForNMI

    lda #$FF
    sta ScoreAnim_Direction
    sta ScoreAnim_DirectionIrq
    sta ScoreAnim_Scroll

    SetIRQ 35, IRQ_Scores
    jsr WaitForIRQ

FrameScores:
    lda ScoreAnim_DirectionIrq
    bmi :+
    jsr WaitForIRQ
    jmp FrameScores
:

    jsr ReadControllers

    lda #BUTTON_B ; B
    jsr ButtonPressed
    beq :+

    lda #InitIndex::Menu
    jmp GotoInit
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :++

    inc Save_CurrentList
    lda Save_CurrentList
    cmp #HS_LIST_COUNT
    bne :+
    lda #0
    sta Save_CurrentList
:

    jsr LoadScores
    lda #0
    sta ScoreAnim_Direction
    sta ScoreAnim_DirectionIrq
    lda #0
    sta ScoreAnim_Frame
:
    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :++

    dec Save_CurrentList
    bpl :+
    lda #HS_LIST_COUNT-1
    sta Save_CurrentList
:

    jsr LoadScores
    lda #1
    sta ScoreAnim_Direction
    sta ScoreAnim_DirectionIrq
    lda #0
    sta ScoreAnim_Frame
:

    jsr WaitForIRQ
    jmp FrameScores

; If the score is a new high score, its index will be in
; NewHsIndex, otherwise $FF is written to this variable.
CheckForNewHighScore:
    lda CurrentGameMode+GameMode::HsIndex
    asl a
    tax
    lda SaveTypeList+0, x
    sta AddressPointer1+0
    lda SaveTypeList+1, x
    sta AddressPointer1+1

    ; start checking from the end of the list
    ldy #.sizeof(ScoreEntry)*(HS_SAVE_COUNT-1)+ScoreEntry::Score

    lda #10
    sta TmpX

@loop:
    lda CurrentScore+ScoreEntry::Score+0
    cmp (AddressPointer1), y
    iny
    lda CurrentScore+ScoreEntry::Score+1
    sbc (AddressPointer1), y
    iny
    lda CurrentScore+ScoreEntry::Score+2
    sbc (AddressPointer1), y
    bcs @next
    jmp @done

@next:
    dey
    dey

    dec TmpX
    beq @done

    tya
    sec
    sbc #.sizeof(ScoreEntry)
    tay
    jmp @loop

@done:

    lda TmpX
    cmp #10
    bne :+
    ; didn't find a new high score
    lda #$FF
    sta NewHsIndex
    rts
:
    sta NewHsIndex
    rts ; TmpX is in A


SetNewHighScore:
    lda CurrentGameMode+GameMode::HsIndex
    asl a
    tax
    lda SaveTypeList+0, x
    sta AddressPointer1+0
    lda SaveTypeList+1, x
    sta AddressPointer1+1

    ; new high score
    lda NewHsIndex
    sta MMC5_MultA
    lda #.sizeof(ScoreEntry)
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    sta TmpY
    adc AddressPointer1+0
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    clc
    lda AddressPointer1+0
    adc #.sizeof(ScoreEntry)
    sta AddressPointer2+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer2+1

    EnableRam
    lda NewHsIndex
    cmp #9
    beq @noMemCpy

    sec
    lda #9
    sbc NewHsIndex
    sta MMC5_MultA
    lda #.sizeof(ScoreEntry)
    sta MMC5_MultB
    ldy MMC5_MultA

    jsr MemCopyRev

@noMemCpy:
    lda AddressPointer1+0
    sta AddressPointer2+0
    lda AddressPointer1+1
    sta AddressPointer2+1

    ldy #.sizeof(ScoreEntry)

    lda #.lobyte(CurrentScore)
    sta AddressPointer1+0
    lda #.hibyte(CurrentScore)
    sta AddressPointer1+1

    jsr MemCopyRev

    DisableRam

    lda #1
    rts

IRQ_Scores:
    lda ScoreAnim_Scroll
    bmi @rts

    ldx ScoreAnim_Scroll

    lda ScoreAnim_DirectionIrq
    bne :+
    lda Scores_Scroll, x
    jmp :++
:
    lda PpuControl
    eor #$01
    sta $2000

    lda Scores_ScrollRev, x
:

    bit $2002
    sta $2005

    inc ScoreAnim_Scroll
    lda ScoreAnim_Scroll
    cmp #Scores_Scroll_Len
    bne @rts
    lda #$FF
    sta ScoreAnim_Scroll
    sta ScoreAnim_DirectionIrq

    ldx PpuControl
    inx
    txa
    and #%1111_1101
    sta PpuControl

@rts:
    rts

NMI_Scores:
    ;jsr BareNmiHandler

    lda ScoreAnim_Direction
    bmi @rts
    lda ScoreAnim_Direction
    bne @left

    lda ScoreAnim_Frame
    bne :+
    lda #0
    sta ScoreAnim_Scroll

    inc ScoreAnim_Frame
    jmp DrawScores_VerticalCol1

:   cmp #1
    bne :+
    inc ScoreAnim_Frame
    jmp DrawScores_VerticalCol2

:   cmp #2
    bne :+
    inc ScoreAnim_Frame
    jmp DrawScores_VerticalCol3
:
    lda #$FF
    sta ScoreAnim_Direction
    jmp @rts


@left:
    lda ScoreAnim_Frame
    bne :+
    lda #0
    sta ScoreAnim_Scroll

    inc ScoreAnim_Frame
    jmp DrawScores_VerticalCol3

:   cmp #1
    bne :+
    inc ScoreAnim_Frame
    jmp DrawScores_VerticalCol2

:   cmp #2
    bne :+
    inc ScoreAnim_Frame
    jmp DrawScores_VerticalCol1
:
    lda #$FF
    sta ScoreAnim_Direction

@rts:
    rts

DrawScores_VerticalCol1:
    lda PpuControl
    and #$1
    beq :+
    lda #.lobyte(HS_NAME_START)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START)
    sta AddressPointer1+1
    jmp :++
:

    lda #.lobyte(HS_NAME_START+1024)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START+1024)
    sta AddressPointer1+1
:
    lda PpuControl
    ora #%0000_0100
    sta $2000

    ldx #0
    bit $2002
@loop:
    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    .repeat 10, i
    lda Scores_Screen+ScoreDisplay::Name+(i*.sizeof(ScoreDisplay)), x
    sta $2007

    lda Scores_Screen+ScoreDisplay::Time+(i*.sizeof(ScoreDisplay)), x
    sta $2007
    .endrepeat

    inc AddressPointer1+0
    bne :+
    inc AddressPointer1+1
:

    inx
    cpx #8
    beq :+
    jmp @loop
:

    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    ldx #' '
    .repeat 10, i
    lda Scores_Screen+ScoreDisplay::Name+(i*.sizeof(ScoreDisplay))+8
    sta $2007

    stx $2007
    .endrepeat

    rts

HS_Col2_Offset = 9
DrawScores_VerticalCol2:
    lda PpuControl
    and #$1
    beq :+
    lda #.lobyte(HS_NAME_START+HS_Col2_Offset)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START+HS_Col2_Offset)
    sta AddressPointer1+1
    jmp :++
:

    lda #.lobyte(HS_NAME_START+1024+HS_Col2_Offset)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START+1024+HS_Col2_Offset)
    sta AddressPointer1+1
:

    lda PpuControl
    ora #%0000_0100
    sta $2000

    ldx #0
    bit $2002
@loop:
    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    .repeat 10, i
    lda Scores_Screen+ScoreDisplay::Name+(i*.sizeof(ScoreDisplay))+HS_Col2_Offset, x
    sta $2007

    lda Scores_Screen+ScoreDisplay::Lines+(i*.sizeof(ScoreDisplay)), x
    sta $2007
    .endrepeat

    inc AddressPointer1+0
    bne :+
    inc AddressPointer1+1
:

    inx
    cpx #7
    beq :+
    jmp @loop
:

    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    ldx #' '
    .repeat 10, i
    stx $2007

    lda Scores_Screen+ScoreDisplay::Lines+(i*.sizeof(ScoreDisplay))+7
    sta $2007
    .endrepeat
    rts

HS_Col3_Offset = 18
DrawScores_VerticalCol3:
    bit $2002

    lda PpuControl
    and #$01
    beq :+

    lda #.hibyte(HS_TITLE_ADDR)
    sta $2006
    lda #.lobyte(HS_TITLE_ADDR)
    sta $2006
    jmp :++
:

    lda #.hibyte(HS_TITLE_ADDR+1024)
    sta $2006
    lda #.lobyte(HS_TITLE_ADDR+1024)
    sta $2006
:

    .repeat HS_TITLE_LEN, i
        lda Title_Screen+i
        sta $2007
    .endrepeat

    lda PpuControl
    and #$1
    beq :+
    lda #.lobyte(HS_NAME_START+HS_Col3_Offset)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START+HS_Col3_Offset)
    sta AddressPointer1+1
    jmp :++
:

    lda #.lobyte(HS_NAME_START+1024+HS_Col3_Offset)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START+1024+HS_Col3_Offset)
    sta AddressPointer1+1
:

    lda PpuControl
    ora #%0000_0100
    sta $2000

    ldx #0
    ldy #' '
    bit $2002
@loop:
    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    .repeat 10, i
    sty $2007

    lda Scores_Screen+ScoreDisplay::Score+(i*.sizeof(ScoreDisplay)), x
    sta $2007
    .endrepeat

    inc AddressPointer1+0
    bne :+
    inc AddressPointer1+1
:

    inx
    cpx #8
    beq :+
    jmp @loop
:
    rts

; A - source index
; AddressPointer2 - destination
LoadScores:
    lda #.lobyte(Scores_Screen)
    sta AddressPointer2+0
    lda #.hibyte(Scores_Screen)
    sta AddressPointer2+1

    lda Save_CurrentList
    asl a
    tax

    lda SaveTypeTitles+0, x
    sta AddressPointer1+0
    lda SaveTypeTitles+1, x
    sta AddressPointer1+1

    ;
    ; Title
    ldy #0
@titleCopy:
    lda (AddressPointer1), y
    beq :+
    sta Title_Screen, y
    iny
    jmp @titleCopy
:
    lda #' '
@titleClear:
    cpy #HS_TITLE_LEN
    beq :+
    sta Title_Screen, y
    iny
    jmp @titleClear
:

    lda SaveTypeList+0, x
    sta AddressPointer1+0
    lda SaveTypeList+1, x
    sta AddressPointer1+1

    lda #HS_SAVE_COUNT
    sta TmpX ; Current entry index

@entry:
    ldy #0
    ;
    ; Name
@copyA:
    lda (AddressPointer1), y
    sta (AddressPointer2), y
    iny
    cpy #.sizeof(ScoreEntry::Name)
    bne @copyA

    ;
    ; Time
    ldy #ScoreEntry::Time+1 ; minutes
    lda (AddressPointer1), y
    jsr BinToDec_8bit

    ldy #ScoreDisplay::Time

    lda #' '
    sta (AddressPointer2), y
    iny
    sta (AddressPointer2), y
    iny

    lda bcdOutput+0
    sta (AddressPointer2), y
    iny
    lda bcdOutput+1
    sta (AddressPointer2), y
    iny
    lda bcdOutput+2
    sta (AddressPointer2), y
    iny

    lda #':'
    sta (AddressPointer2), y
    iny
    tya
    pha

    ldy #ScoreEntry::Time+0 ; seconds
    lda (AddressPointer1), y
    jsr BinToDec_8bit

    pla
    tay
    lda bcdOutput+1
    cmp #' '
    bne :+
    lda #'0'
:
    sta (AddressPointer2), y
    iny
    lda bcdOutput+2
    sta (AddressPointer2), y

    ;
    ; Lines
    ldx #0
    ldy #ScoreEntry::Lines
@copyC:
    lda (AddressPointer1), y
    sta bcdInput, x
    iny
    inx
    cpx #.sizeof(ScoreEntry::Lines)
    bne @copyC

    jsr BinToDec_Shift

    ldy #ScoreDisplay::Lines
    ldx #0
@copyD:
    lda bcdOutput, x
    sta (AddressPointer2), y
    iny
    inx
    cpx #.sizeof(ScoreDisplay::Lines)
    bne @copyD

    ;
    ; Score
    ldx #0
    ldy #ScoreEntry::Score
@copyE:
    lda (AddressPointer1), y
    sta bcdInput, x
    iny
    inx
    cpx #.sizeof(ScoreEntry::Score)
    bne @copyE

    jsr BinToDec_Shift

    ldy #ScoreDisplay::Score
    ldx #0
@copyF:
    lda bcdOutput, x
    sta (AddressPointer2), y
    iny
    inx
    cpx #.sizeof(ScoreDisplay::Score)
    bne @copyF

    clc
    lda AddressPointer1+0
    adc #.sizeof(ScoreEntry) ; .sizeof(ScoreEntry)
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    clc
    lda AddressPointer2+0
    adc #.sizeof(ScoreDisplay) ; .sizeof(ScoreDisplay)
    sta AddressPointer2+0
    lda AddressPointer2+1
    adc #0
    sta AddressPointer2+1

    dec TmpX
    beq :+
    jmp @entry
:   rts

Palette_EnterNameSp:
    .byte $0F, $13, $0F, $0F

EnTbl_AddrScore = $2283
EnTbl_AddrLines = $2293
EnTbl_AddrTime  = $22EB

InitScores_EnterName:
    lda #%1000_0000
    sta PpuControl

    lda #.lobyte(Save_Palettes)
    sta AddressPointer1+0
    lda #.hibyte(Save_Palettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes

    lda #.lobyte(Palette_EnterNameSp)
    sta AddressPointer1+0
    lda #.hibyte(Palette_EnterNameSp)
    sta AddressPointer1+1
    ldx #4
    jsr LoadPalette

    jsr WaitForNMI

    lda #0
    sta EN_Shifted

    ; attributes
    lda #%0010_0000
    .repeat 4, i
        sta EN_SelectSprites+2+(i*4)
    .endrepeat
    sta EN_CursorSprite+2

    ; tiles
    lda #$8E
    sta EN_SelectSprites+1+(0*4)
    lda #$8F
    sta EN_SelectSprites+1+(1*4)
    lda #$9E
    sta EN_SelectSprites+1+(2*4)
    lda #$9F
    sta EN_SelectSprites+1+(3*4)

    lda #$84
    sta EN_CursorSprite+1

    lda #EnTbl::One
    sta EN_Selection

    lda #%0000_0010
    sta $5104

    ldx #%1100_0000
    ldy #%1100_0001
    .repeat 9, i
        stx EnTbl_AddrScore+i+MMC5_OFFSET
        sty EnTbl_AddrScore+i+32+MMC5_OFFSET
    .endrepeat

    ldx #%1000_0000
    ldy #%1000_0001
    .repeat 9, i
        stx EnTbl_AddrLines+i+MMC5_OFFSET
        sty EnTbl_AddrLines+i+32+MMC5_OFFSET
    .endrepeat

    ldx #%0100_0000
    ldy #%0100_0001
    .repeat 9, i
        stx EnTbl_AddrTime+i+MMC5_OFFSET
        sty EnTbl_AddrTime+i+32+MMC5_OFFSET
    .endrepeat

    lda #%0000_0001
    sta $5104

    .repeat .sizeof(ScoreEntry::Score), i
        lda CurrentScore+ScoreEntry::Score+i
        sta bcdInput+i
    .endrepeat
    jsr BinToDec_Shift

    lda #.hibyte(EnTbl_AddrScore+32)
    sta $2006
    lda #.lobyte(EnTbl_AddrScore+32)
    sta $2006

    .repeat 8, i
        lda bcdOutput+i
        sta $2007
    .endrepeat

    .repeat .sizeof(ScoreEntry::Lines), i
        lda CurrentScore+ScoreEntry::Lines+i
        sta bcdInput+i
    .endrepeat
    jsr BinToDec_Shift

    lda #.hibyte(EnTbl_AddrLines+32)
    sta $2006
    lda #.lobyte(EnTbl_AddrLines+32)
    sta $2006

    .repeat 8, i
        lda bcdOutput+i
        sta $2007
    .endrepeat

    lda #.hibyte(EnTbl_AddrTime+32+1)
    sta $2006
    lda #.lobyte(EnTbl_AddrTime+32+1)
    sta $2006

    lda #' ' ; hour
    sta $2007
    sta $2007
    ;lda #':'
    ;sta $2007

    ; minute
    lda CurrentScore+ScoreEntry::Time+1
    jsr BinToDec_8bit

    lda bcdOutput+0
    sta $2007
    lda bcdOutput+1
    sta $2007
    lda bcdOutput+2
    sta $2007

    lda #':'
    sta $2007

    ; seconds
    lda CurrentScore+ScoreEntry::Time+0
    jsr BinToDec_8bit

    lda bcdOutput+1
    cmp #' '
    bne :+
    lda #'0'
:
    sta $2007
    lda bcdOutput+2
    sta $2007

    lda #.lobyte(nmiName)
    sta NmiHandler+0
    lda #.hibyte(nmiName)
    sta NmiHandler+1

    lda #%0001_1110
    sta PpuMask
    jsr WaitForNMI

Frame_EnterName:
    SetIRQ 48, irqNameTop

    jsr WaitForIRQ

    jsr ReadControllers

    lda #BUTTON_SELECT ; select
    jsr ButtonPressed
    beq :+
    inc EN_Shifted
    lda EN_Shifted
    and #$01
    sta EN_Shifted
:

    lda #BUTTON_START ; start
    jsr ButtonPressed
    beq :++
    lda EN_Selection
    cmp #EnTbl::End
    beq :+
    lda #EnTbl::End
    sta EN_Selection
    jmp :++
:
    jsr EN_DoSelect
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :+
    lda #EnDir::Left
    jsr EN_UpdateSelection
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    lda #EnDir::Right
    jsr EN_UpdateSelection
:

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    lda #EnDir::Up
    jsr EN_UpdateSelection
:

    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    lda #EnDir::Down
    jsr EN_UpdateSelection
:

    lda #BUTTON_A ; a
    jsr ButtonPressed
    beq :+
    jsr EN_DoSelect
:

    lda #BUTTON_B ; b
    jsr ButtonPressed
    beq :+
    jsr EN_Backspace
:

    jsr EN_UpdateSprites

    lda #%0001_1000
    sta PpuMask
    jsr WaitForNMI

    jmp Frame_EnterName

EnTbl_Directions:
    ;     Up,           Down,             Left,         Right
    .byte EnTbl::LtrQ,  EnTbl::LtrZ,      EnTbl::LtrA,  EnTbl::LtrS ; A
    .byte EnTbl::LtrH,  EnTbl::Space,     EnTbl::LtrV,  EnTbl::LtrN ; B
    .byte EnTbl::LtrF,  EnTbl::Space,     EnTbl::LtrX,  EnTbl::LtrV ; C
    .byte EnTbl::LtrE,  EnTbl::LtrX,      EnTbl::LtrS,  EnTbl::LtrF ; D
    .byte EnTbl::Four,  EnTbl::LtrD,      EnTbl::LtrW,  EnTbl::LtrR ; E
    .byte EnTbl::LtrR,  EnTbl::LtrC,      EnTbl::LtrD,  EnTbl::LtrG ; F
    .byte EnTbl::LtrT,  EnTbl::LtrV,      EnTbl::LtrF,  EnTbl::LtrH ; G
    .byte EnTbl::LtrY,  EnTbl::LtrB,      EnTbl::LtrG,  EnTbl::LtrJ ; H
    .byte EnTbl::Nine,  EnTbl::LtrK,      EnTbl::LtrU,  EnTbl::LtrO ; I
    .byte EnTbl::LtrU,  EnTbl::LtrN,      EnTbl::LtrH,  EnTbl::LtrK ; J
    .byte EnTbl::LtrI,  EnTbl::LtrM,      EnTbl::LtrJ,  EnTbl::LtrL ; K
    .byte EnTbl::LtrO,  EnTbl::Comma,     EnTbl::LtrK,  EnTbl::Semicolon ; L
    .byte EnTbl::LtrK,  EnTbl::Space,     EnTbl::LtrN,  EnTbl::Comma ; M
    .byte EnTbl::LtrJ,  EnTbl::Space,     EnTbl::LtrB,  EnTbl::LtrM ; N
    .byte EnTbl::Zero,  EnTbl::LtrL,      EnTbl::LtrI,  EnTbl::LtrP ; O
    .byte EnTbl::Dash,  EnTbl::Semicolon, EnTbl::LtrO,  EnTbl::BracketLeft ; P
    .byte EnTbl::Two,   EnTbl::LtrA,      EnTbl::Tilde, EnTbl::LtrW ; Q
    .byte EnTbl::Five,  EnTbl::LtrF,      EnTbl::LtrE,  EnTbl::LtrT ; R
    .byte EnTbl::LtrW,  EnTbl::LtrZ,      EnTbl::LtrA,  EnTbl::LtrD ; S
    .byte EnTbl::Six,   EnTbl::LtrG,      EnTbl::LtrR,  EnTbl::LtrY ; T
    .byte EnTbl::Eight, EnTbl::LtrJ,      EnTbl::LtrY,  EnTbl::LtrI ; U
    .byte EnTbl::LtrG,  EnTbl::Space,     EnTbl::LtrC,  EnTbl::LtrB ; V
    .byte EnTbl::Three, EnTbl::LtrS,      EnTbl::LtrQ,  EnTbl::LtrE ; W
    .byte EnTbl::LtrD,  EnTbl::Space,     EnTbl::LtrZ,  EnTbl::LtrC ; X
    .byte EnTbl::Seven, EnTbl::LtrH,      EnTbl::LtrT,  EnTbl::LtrU ; Y
    .byte EnTbl::LtrS,  EnTbl::Space,     EnTbl::LtrZ,  EnTbl::LtrX ; Z

    .byte EnTbl::One,          EnTbl::Tilde,        EnTbl::One,          EnTbl::Two ; 1
    .byte EnTbl::Two,          EnTbl::LtrQ,         EnTbl::One,          EnTbl::Three ; 2
    .byte EnTbl::Three,        EnTbl::LtrW,         EnTbl::Two,          EnTbl::Four ; 3
    .byte EnTbl::Four,         EnTbl::LtrE,         EnTbl::Three,        EnTbl::Five ; 4
    .byte EnTbl::Five,         EnTbl::LtrR,         EnTbl::Four,         EnTbl::Six ; 5
    .byte EnTbl::Six,          EnTbl::LtrT,         EnTbl::Five,         EnTbl::Seven ; 6
    .byte EnTbl::Seven,        EnTbl::LtrY,         EnTbl::Six,          EnTbl::Eight ; 7
    .byte EnTbl::Eight,        EnTbl::LtrU,         EnTbl::Seven,        EnTbl::Nine ; 8
    .byte EnTbl::Nine,         EnTbl::LtrI,         EnTbl::Eight,        EnTbl::Zero ; 9
    .byte EnTbl::Zero,         EnTbl::LtrO,         EnTbl::Nine,         EnTbl::Dash ; 0
    .byte EnTbl::Dash,         EnTbl::LtrP,         EnTbl::Zero,         EnTbl::Equal ; dash
    .byte EnTbl::Equal,        EnTbl::BracketLeft,  EnTbl::Dash,         EnTbl::Equal ; equal

    .byte EnTbl::LtrZ,         EnTbl::Erase,        EnTbl::Erase,        EnTbl::Space ; erase
    .byte EnTbl::LtrB,         EnTbl::Space,        EnTbl::Erase,        EnTbl::End ; space
    .byte EnTbl::Slash,        EnTbl::End,          EnTbl::Space,        EnTbl::End ; end

    .byte EnTbl::One,          EnTbl::LtrA,         EnTbl::Tilde,        EnTbl::LtrQ ; tilde
    .byte EnTbl::Equal,        EnTbl::Quote,        EnTbl::LtrP,         EnTbl::BracketRight ; [
    .byte EnTbl::Equal,        EnTbl::Pipe,         EnTbl::BracketLeft,  EnTbl::BracketRight ; ]
    .byte EnTbl::BracketRight, EnTbl::Slash,        EnTbl::Quote,        EnTbl::Pipe ; |
    .byte EnTbl::LtrP,         EnTbl::Period,       EnTbl::LtrL,         EnTbl::Quote ; ;
    .byte EnTbl::BracketLeft,  EnTbl::Slash,        EnTbl::Semicolon,    EnTbl::Pipe ; '
    .byte EnTbl::LtrL,         EnTbl::Space,        EnTbl::LtrM,         EnTbl::Period ; ,
    .byte EnTbl::Semicolon,    EnTbl::Space,        EnTbl::Comma,        EnTbl::Slash ; .
    .byte EnTbl::Quote,        EnTbl::Space,        EnTbl::Period,       EnTbl::Slash ; /

EnTbl_Values:
    .byte "aA"
    .byte "bB"
    .byte "cC"
    .byte "dD"
    .byte "eE"
    .byte "fF"
    .byte "gG"
    .byte "hH"
    .byte "iI"
    .byte "jJ"
    .byte "kK"
    .byte "lL"
    .byte "mM"
    .byte "nN"
    .byte "oO"
    .byte "pP"
    .byte "qQ"
    .byte "rR"
    .byte "sS"
    .byte "tT"
    .byte "uU"
    .byte "vV"
    .byte "wW"
    .byte "xX"
    .byte "yY"
    .byte "zZ"
    .byte "1!"
    .byte "2@"
    .byte "3#"
    .byte "4$"
    .byte "5%"
    .byte "6^"
    .byte "7&"
    .byte "8*"
    .byte "9("
    .byte "0)"
    .byte "-_"
    .byte "=+"
    .byte $08, $08 ; backspace
    .byte "  "
    .byte $03, $03 ; end
    .byte "`~"
    .byte "[{"
    .byte "]}"
    .byte "|\"
    .byte ";:"
    .byte "'", '"'
    .byte ",<"
    .byte ".>"
    .byte "/?"

ENSP_HOME_X = 20
ENSP_HOME_Y = 59
.macro ENSP cY, cX
    .byte ENSP_HOME_X+(16*(cX-1))
    .byte ENSP_HOME_Y+(16*(cY-1))
.endmacro

EnTbl_Sprites:
;     Row, Col
    ENSP 3, 2 ; A
    ENSP 4, 7 ; B
    ENSP 4, 5 ; C
    ENSP 3, 4 ; D
    ENSP 2, 4 ; E
    ENSP 3, 5 ; F
    ENSP 3, 6 ; G
    ENSP 3, 7 ; H
    ENSP 2, 9 ; I
    ENSP 3, 8 ; J
    ENSP 3, 9 ; K
    ENSP 3, 10; L
    ENSP 4, 9 ; M
    ENSP 4, 8 ; N
    ENSP 2, 10; O
    ENSP 2, 11; P
    ENSP 2, 2 ; Q
    ENSP 2, 5 ; R
    ENSP 3, 3 ; S
    ENSP 2, 6 ; T
    ENSP 2, 8 ; U
    ENSP 4, 6 ; V
    ENSP 2, 3 ; W
    ENSP 4, 4 ; X
    ENSP 2, 7 ; Y
    ENSP 4, 3 ; Z

    ; numbers
    .repeat 10, i
        ENSP 1, i+1
    .endrepeat

    ENSP 1, 11 ; dash
    ENSP 1, 12 ; equal
    ENSP 5, 1  ; erase
    ENSP 5, 6  ; space
    ENSP 5, 12 ; end

    ENSP 2, 1 ; tilde
    ENSP 2, 12 ; [
    ENSP 2, 13 ; ]
    ENSP 3, 13 ; |
    ENSP 3, 11 ; ;
    ENSP 3, 12 ; '
    ENSP 4, 10 ; ,
    ENSP 4, 11 ; .
    ENSP 4, 12 ; /

EN_Backspace:
    ldx EN_Cursor
    cpx #.sizeof(ScoreEntry::Name)-1
    bne :+
    lda CurrentScore+ScoreEntry::Name, x
    beq :+
    inx
:
    dex
    bpl :+
    ldx #0
:
    stx EN_Cursor

    lda #$00
    sta CurrentScore+ScoreEntry::Name, x
    rts

EN_DoSelect:
    lda EN_Selection
    asl a
    tax
    lda EN_Shifted
    beq :+
    inx
:
    lda EnTbl_Values, x
    cmp #$08 ; backspace
    beq EN_Backspace

    cmp #$03 ; end
    bne :+
    jsr SetNewHighScore
    lda #InitIndex::Scores
    jmp GotoInit
:

    ldx EN_Cursor
    sta CurrentScore+ScoreEntry::Name, x
    inx
    cpx #.sizeof(ScoreEntry::Name)
    bcc :+
    ldx #.sizeof(ScoreEntry::Name)-1
:
    stx EN_Cursor
    rts

EN_UpdateSelection:
    sta TmpX
    lda EN_Selection
    asl a
    asl a
    clc
    adc TmpX
    tax
    lda EnTbl_Directions, x
    sta EN_Selection
    rts

EN_CURSOR_HOME_X = 64
EN_CURSOR_HOME_Y = 39
EN_UpdateSprites:
    ;
    ; Keyboard Selection
    lda EN_Selection
    asl a
    tax

    lda EnTbl_Sprites+0, x
    sta TmpX
    lda EnTbl_Sprites+1, x
    sta TmpY

    lda TmpX
    sta EN_SelectSprites+3+(0*4)
    lda TmpY
    sta EN_SelectSprites+0+(0*4)

    clc
    lda TmpX
    adc #8
    sta EN_SelectSprites+3+(1*4)
    lda TmpY
    sta EN_SelectSprites+0+(1*4)

    clc
    lda TmpX
    sta EN_SelectSprites+3+(2*4)
    lda TmpY
    adc #8
    sta EN_SelectSprites+0+(2*4)

    clc
    lda TmpX
    adc #8
    sta EN_SelectSprites+3+(3*4)
    lda TmpY
    adc #8
    sta EN_SelectSprites+0+(3*4)

    ;
    ; Cursor
    lda #EN_CURSOR_HOME_Y
    sta EN_CursorSprite+0

    lda EN_Cursor
    asl a
    asl a
    asl a
    clc
    adc #EN_CURSOR_HOME_X
    sta EN_CursorSprite+3
    rts

irqNameTop:
    SetIRQ 123, irqNameBottom

    lda PpuControl
    and #%1111_1100
    ora EN_Shifted
    sta PpuControl
    sta $2000

    lda #0
    sta $2005
    sta $2005
    rts

irqNameBottom:
    lda PpuControl
    and #%1111_1100
    sta PpuControl
    sta $2000
    lda #0
    sta $2005
    sta $2005
    rts

EN_CURSOR_PPU = $20A8
nmiName:
    jsr BareNmiHandler
    lda #.hibyte(EN_CURSOR_PPU)
    sta $2006
    lda #.lobyte(EN_CURSOR_PPU)
    sta $2006

    .repeat .sizeof(ScoreEntry::Name), i
    lda CurrentScore+ScoreEntry::Name+i
    bne :+
    lda #'_'
:
    sta $2007
    .endrepeat

    ;lda #0
    ;sta NmiHandler+0
    ;sta NmiHandler+1
    rts

DummyData:
    .byte "< one          >"
    .byte 0, 0    ; time
    .byte 10, 0, 0 ; lines
    .byte 10, 0, 0 ; score

    .byte "< two          >"
    .byte 0, 0
    .byte 9, 0, 0
    .byte 9, 0, 0

    .byte "< three        >"
    .byte 0, 0
    .byte 8, 0, 0
    .byte 8, 0, 0

    .byte "< four         >"
    .byte 0, 0
    .byte 7, 0, 0
    .byte 7, 0, 0

    .byte "< five         >"
    .byte 0, 0
    .byte 6, 0, 0
    .byte 6, 0, 0

    .byte "< six          >"
    .byte 0, 0
    .byte 5, 0, 0
    .byte 5, 0, 0

    .byte "< seven        >"
    .byte 0, 0
    .byte 4, 0, 0
    .byte 4, 0, 0

    .byte "< eight        >"
    .byte 0, 0
    .byte 3, 0, 0
    .byte 3, 0, 0

    .byte "< nine         >"
    .byte 0, 0
    .byte 2, 0, 0
    .byte 2, 0, 0

    .byte "< ten          >"
    .byte 0, 0
    .byte 1, 0, 0 ; lines
    .byte 1, 0, 0 ; score

.popseg
