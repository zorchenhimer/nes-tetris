.pushseg
.segment "BSS"
PB_Length: .res 1
.popseg

.struct PBEntry
    Name .byte 2
    Data .byte 2
.endstruct

PBTitle:
    .asciiz "Practice Modes"

PracticeMenuData:
    .asciiz "Practice Modes"

    .word :+
    .byte BasicMenu::ItemType::Button
    .word Practice_PrevMenu

    ; TODO: figure out a better format to auto-generate
    .word PB_name_3AA2F797
    .byte BasicMenu::ItemType::Button
    .word Practice_DoButton

    .word PB_name_DF81ACF2
    .byte BasicMenu::ItemType::Button
    .word Practice_DoButton

    .word PB_name_3E786613
    .byte BasicMenu::ItemType::Button
    .word Practice_DoButton

    .word $0000

:   .asciiz "<- Back"

Practice_PrevMenu:
    lda #InitIndex::Modes
    jmp GotoInit

InitPracticeMenu:
    lda #.lobyte(OptPal_Off)
    sta AddressPointer1+0
    lda #.hibyte(OptPal_Off)
    sta AddressPointer1+1
    ldx #0
    jsr LoadPalette

    lda #.lobyte(OptPal_On)
    sta AddressPointer1+0
    lda #.hibyte(OptPal_On)
    sta AddressPointer1+1
    ldx #1
    jsr LoadPalette

    lda #.lobyte(PracticeMenuData)
    sta BasicMenu::MenuData+0
    lda #.hibyte(PracticeMenuData)
    sta BasicMenu::MenuData+1

    jmp BasicMenu::Init

Practice_DoButton:
    brk ; TODO
    rts

.include "practice-boards.i"
