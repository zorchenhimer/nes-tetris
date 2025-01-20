.struct ScoreEntry
    Name  .byte 16
    Time  .byte 2
    Lines .byte 3
    Score .byte 3 ; maybe something else?
.endstruct

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

.out .sprintf(".sizeof(ScoreEntry): %d", .sizeof(ScoreEntry))
.out .sprintf("HS_LIST_SIZE: %d", HS_LIST_SIZE)
.out .sprintf("HS_DISPLAY_SIZE: %d", HS_DISPLAY_SIZE)

.pushseg
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

.segment "BSS"
Save_CurrentList: .res 1

Scores_Screen: .res HS_DISPLAY_SIZE
Title_Screen: .res HS_TITLE_LEN

;Scores_A: .res .sizeof(ScoreEntry)
;Scores_B: .res .sizeof(ScoreDisplay)

ScoreAnim_Direction: .res 1 ; 0 = right; 1 = left; 256 = nothing
ScoreAnim_DirectionIrq: .res 1 ; 0 = right; 1 = left; 256 = nothing
ScoreAnim_Frame:     .res 1
ScoreAnim_Scroll:    .res 1
Score_DrawTitle:     .res 1

.popseg

Save_CheckVal_Check:
    .byte "Zorch"

Save_Palettes:
    .byte $0F, $20, $00, $10
    .byte $0F, $21, $00, $10
    .byte $0F, $25, $00, $10
    .byte $0F, $2B, $00, $10

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
    ;jsr Save_ClearTable
    jsr Save_ResetTable
.endmacro

InitRam:
    ldx #0
@loop:
    lda Save_CheckVal, x
    cmp Save_CheckVal_Check, x
    bne @reset
    inx
    cpx #5
    bne @loop
    rts
@reset:

    lda #$02
    sta $5102
    lda #$01
    sta $5103

    lda #GHOST_FLASH
    sta Option_GhostFlash
    lda #SCREEN_SHAKE
    sta Option_ScreenShake
    lda #BUTTON_REPEAT_START
    sta Option_ShiftStart
    lda #BUTTON_REPEAT
    sta Option_ShiftRepeat

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

    lda #$00
    sta $5102
    sta $5103
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
    jsr ClearSprites

    lda #0
    sta $2000

    ldx #0
    jsr FillAttributeTable

    lda #.lobyte(Save_Palettes)
    sta AddressPointer1+0
    lda #.hibyte(Save_Palettes)
    sta AddressPointer1+1
    jsr LoadBgPalettes

    jsr ClearExtAttr

    lda #%0000_0010
    sta $5104

    ;
    ; Time colors
    ldx #0
    lda #%0100_0000
:
    .repeat 11, i
        sta MMC5_OFFSET+HS_NAME_START-32+(i*64), x
    .endrepeat

    inx
    cpx #8
    bne :-

    ;
    ; Lines colors
    ldx #0
    lda #%1000_0000
:
    .repeat 11, i
        sta MMC5_OFFSET+HS_NAME_START-32+9+(i*64), x
    .endrepeat

    inx
    cpx #8
    bne :-

    ;
    ; Scores colors
    ldx #0
    lda #%1100_0000
:
    .repeat 11, i
        sta MMC5_OFFSET+HS_NAME_START-32+18+(i*64), x
    .endrepeat

    inx
    cpx #8
    bne :-

    lda #%0000_0001
    sta $5104

    ;ldx #0
    ;jsr FillScreen
    lda #.lobyte(Screen_Scores)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Scores)
    sta AddressPointer1+1
    lda #$20
    jsr DrawScreen_RLE

    lda #.lobyte(Screen_Scores)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Scores)
    sta AddressPointer1+1
    lda #$24
    jsr DrawScreen_RLE

    lda #0 ; Index of list
    sta Save_CurrentList

    jsr LoadScores
    jsr DrawScores

    lda #%1000_0000
    sta PpuControl
    sta $2000

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001
    jsr WaitForNMI

    lda #$FF
    sta ScoreAnim_Direction
    sta ScoreAnim_DirectionIrq
    sta ScoreAnim_Scroll

    lda #.lobyte(NMI_Scores)
    sta NmiHandler+0
    lda #.hibyte(NMI_Scores)
    sta NmiHandler+1

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

    lda #$00
    sta $5102
    sta $5103
    jsr WaitForNMI
    jmp InitMenu
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

; Currently only draws ScreenA
DrawScores:
    lda #.hibyte(HS_TITLE_ADDR)
    sta $2006
    lda #.lobyte(HS_TITLE_ADDR)
    sta $2006

    ldx #0
:   lda Title_Screen, x
    sta $2007
    inx
    cpx #HS_TITLE_LEN
    bne :-

    lda #.lobyte(HS_NAME_START)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START)
    sta AddressPointer1+1

    lda #.lobyte(Scores_Screen)
    sta AddressPointer2+0
    lda #.hibyte(Scores_Screen)
    sta AddressPointer2+1

    lda #HS_SAVE_COUNT
    sta TmpX

@entry:
    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    ldx #.sizeof(ScoreDisplay::Name)
    ldy #ScoreDisplay::Name
@name:
    lda (AddressPointer2), y
    sta $2007
    iny
    dex
    bne @name

    clc
    lda AddressPointer1+0
    adc #32
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    ldy #ScoreDisplay::Time
    ldx #.sizeof(ScoreDisplay::Time)
:
    lda (AddressPointer2), y
    sta $2007
    iny
    dex
    bne :-

    lda #' '
    sta $2007

    ldx #.sizeof(ScoreDisplay::Lines)
    ldy #ScoreDisplay::Lines
@zeroLines:
    lda (AddressPointer2), y
    cmp #$30
    bne :+
    iny
    lda #' '
    sta $2007
    dex
    cpx #1
    bne @zeroLines
:

@valueLines:
    lda (AddressPointer2), y
    sta $2007
    iny
    dex
    bne @valueLines

    lda #' '
    sta $2007

    ldx #.sizeof(ScoreDisplay::Score)
    ldy #ScoreDisplay::Score
@zeroScore:
    lda (AddressPointer2), y
    cmp #$30
    bne :+
    iny
    lda #' '
    sta $2007
    dex
    cpx #1
    bne @zeroScore
:

@valueScore:
    lda (AddressPointer2), y
    sta $2007
    iny
    dex
    bne @valueScore

    dec TmpX
    beq @done

    clc
    lda AddressPointer1+0
    adc #32
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    clc
    lda AddressPointer2+0
    adc #.sizeof(ScoreDisplay)
    sta AddressPointer2+0
    lda AddressPointer2+1
    adc #0
    sta AddressPointer2+1
    jmp @entry

@done:
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
    ldx #0
    ldy #ScoreEntry::Time
@copyB:
    lda (AddressPointer1), y
    sta bcdInput, x
    iny
    inx
    cpx #.sizeof(ScoreEntry::Time)
    bne @copyB

    ldy #ScoreDisplay::Time
    ldx #0
    lda #'0'
:
    sta (AddressPointer2), y
    iny
    inx
    cpx #.sizeof(ScoreDisplay::Time)
    bne :-

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

DummyData:
    .byte "< one          >"
    .byte 0, 0    ; time
    .byte 1, 0, 0 ; lines
    .byte 2, 0, 0 ; score

    .byte "< two          >"
    .byte 0, 0
    .byte 3, 0, 0
    .byte 4, 0, 0

    .byte "< three        >"
    .byte 0, 0
    .byte 5, 0, 0
    .byte 6, 0, 0

    .byte "< four         >"
    .byte 0, 0
    .byte 7, 0, 0
    .byte 8, 0, 0

    .byte "< five         >"
    .byte 0, 0
    .byte 9, 0, 0
    .byte 10, 0, 0

    .byte "< six          >"
    .byte 0, 0
    .byte 11, 0, 0
    .byte 12, 0, 0

    .byte "< seven        >"
    .byte 0, 0
    .byte 13, 0, 0
    .byte 14, 0, 0

    .byte "< eight        >"
    .byte 0, 0
    .byte 15, 0, 0
    .byte 16, 0, 0

    .byte "< nine         >"
    .byte 0, 0
    .byte 17, 0, 0
    .byte 18, 0, 0

    .byte "< ten          >"
    .byte 0, 0
    .byte 19, 0, 0
    .byte 20, 0, 0
