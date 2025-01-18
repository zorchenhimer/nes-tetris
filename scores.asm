.struct ScoreEntry
    Name   .byte 16
    ValueA .byte 3
    ValueB .byte 3 ; maybe something else?
.endstruct

HS_SAVE_COUNT = 10
HS_LIST_SIZE = .sizeof(ScoreEntry) * HS_SAVE_COUNT

.struct ScoreDisplay
    Name  .byte 16
    Value .byte 8 ; only ValueA
.endstruct

HS_DISPLAY_SIZE = .sizeof(ScoreDisplay) * HS_SAVE_COUNT

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
CurrentList: .res 1

Scores_ScreenA: .res HS_DISPLAY_SIZE
Scores_ScreenB: .res HS_DISPLAY_SIZE

Scores_A: .res .sizeof(ScoreEntry)
;Scores_B: .res .sizeof(ScoreDisplay)

.popseg

Save_CheckVal_Check:
    .byte "Zorch"

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

; Table to clear in AddressPointer1
Save_ClearTable:
    lda #10
    sta TmpX
    ldy #0
@entries:
    ldx #.sizeof(ScoreEntry::Name)
    lda #' '
@name:
    sta (AddressPointer1), y
    iny
    dex
    bne @name

    ldx #.sizeof(ScoreEntry::ValueA)
    lda #0
@val:
    sta (AddressPointer1), y
    iny
    dex
    bne @val

    dec TmpX
    bne @entries
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

SaveTypeNames:
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
:   .asciiz "Time Attack - Lines"
:   .asciiz "Time Attack - Score"
:   .asciiz "Dirty Board"
:   .asciiz "No Hold"
:   .asciiz "Classic"

InitScores:
    jsr ClearSprites

    lda #0
    sta $2000

    ldx #0
    jsr FillAttributeTable

    ;ldx #0
    ;jsr FillScreen
    lda #.lobyte(Screen_Scores)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Scores)
    sta AddressPointer1+1
    jsr DrawScreen_RLE

    jsr ClearExtAttr

    lda #.lobyte(Save_Standard)
    sta AddressPointer1+0
    lda #.hibyte(Save_Standard)
    sta AddressPointer1+1

    lda #.lobyte(Scores_ScreenA)
    sta AddressPointer2+0
    lda #.hibyte(Scores_ScreenA)
    sta AddressPointer2+1

    jsr LoadScores
    jsr DrawScores

    lda #%1000_0000
    sta PpuControl
    sta $2000

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001
    jsr WaitForNMI

FrameScores:
    jsr WaitForNMI
    jmp FrameScores

HS_NAME_START = $21A5
DrawScores:
    lda #.lobyte(HS_NAME_START)
    sta AddressPointer1+0
    lda #.hibyte(HS_NAME_START)
    sta AddressPointer1+1

    lda #HS_SAVE_COUNT
    sta TmpX

    ldy #0
@entry:
    lda AddressPointer1+1
    sta $2006
    lda AddressPointer1+0
    sta $2006

    ldx #.sizeof(ScoreDisplay::Name)
@name:
    lda Scores_ScreenA, y
    sta $2007
    iny
    dex
    bne @name

    lda #' '
    sta $2007

    ldx #.sizeof(ScoreDisplay::Value)
@zero:
    lda Scores_ScreenA, y
    cmp #$30
    bne @value
    iny
    lda #' '
    sta $2007
    dex
    cpx #1
    bne @zero

@value:
    lda Scores_ScreenA, y
    sta $2007
    iny
    dex
    bne @value

    dec TmpX
    beq @done

    clc
    lda AddressPointer1+0
    adc #32
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
    jmp @entry

@done:
    rts

; AddressPointer1 - source
; AddressPointer2 - destination
LoadScores:
    lda #HS_SAVE_COUNT
    sta TmpX ; Current entry index

@entry:
    ldy #0
@copyA:
    lda (AddressPointer1), y
    sta (AddressPointer2), y
    iny
    cpy #.sizeof(ScoreEntry::Name)
    bne @copyA

    ldx #0
@copyB:
    lda (AddressPointer1), y
    sta bcdInput, x
    iny
    inx
    cpx #.sizeof(ScoreEntry::ValueA)
    bne @copyB

    jsr BinToDec_Shift

    clc
    tya
    adc #.sizeof(ScoreEntry::ValueB)
    adc AddressPointer1+0
    sta AddressPointer1+0

    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    ldy #ScoreDisplay::Value
    ldx #0
@copyC:
    lda bcdOutput, x
    sta (AddressPointer2), y
    iny
    inx
    cpx #.sizeof(ScoreDisplay::Value)
    bne @copyC

    clc
    tya
    adc AddressPointer2+0
    sta AddressPointer2+0

    lda AddressPointer2+1
    adc #0
    sta AddressPointer2+1

    dec TmpX
    bne @entry
    rts

DummyData:
    .byte "< one          >"
    .byte 1, 0, 0, 0, 0, 0

    .byte "< two          >"
    .byte 2, 0, 0, 0, 0, 0

    .byte "< three        >"
    .byte 3, 0, 0, 0, 0, 0

    .byte "< four         >"
    .byte 4, 0, 0, 0, 0, 0

    .byte "< five         >"
    .byte 5, 0, 0, 0, 0, 0

    .byte "< six          >"
    .byte 6, 0, 0, 0, 0, 0

    .byte "< seven        >"
    .byte 7, 0, 0, 0, 0, 0

    .byte "< eight        >"
    .byte 8, 0, 0, 0, 0, 0

    .byte "< nine         >"
    .byte 9, 0, 0, 0, 0, 0

    .byte "< ten          >"
    .byte 10, 0, 0, 0, 0, 0
