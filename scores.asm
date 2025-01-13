.struct ScoreEntry
    Name   .byte 16
    Value  .byte 6
.endstruct

HS_SAVE_COUNT = 10
HS_LIST_SIZE = .sizeof(ScoreEntry) * HS_SAVE_COUNT

.out .sprintf(".sizeof(ScoreEntry): %d", .sizeof(ScoreEntry))
.out .sprintf("HS_LIST_SIZE: %d", HS_LIST_SIZE)

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

Scores_ScreenA: .res HS_LIST_SIZE
Scores_ScreenB: .res HS_LIST_SIZE

.popseg

Save_CheckVal_Check:
    .byte "Zorch"

.macro ClearSaveTable addr
    lda #.lobyte(addr)
    sta AddressPointer1+0
    lda #.hibyte(addr)
    sta AddressPointer1+1
    jsr Save_ClearTable
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

    ldx #.sizeof(ScoreEntry::Value)
    lda #0
@val:
    sta (AddressPointer1), y
    iny
    dex
    bne @val

    dec TmpX
    bne @entries
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

; AddressPointer1 - source
; AddressPointer2 - destination
LoadScores:
    ldx #HS_LIST_SIZE
    ldy #0
:   lda (AddressPointer1), y
    sta (AddressPointer2), y
    dex
    bne :-
    rts
