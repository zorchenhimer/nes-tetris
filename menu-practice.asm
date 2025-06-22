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
    .word PB_name_87E7F917
    .byte BasicMenu::ItemType::Button
    .word Practice_DoButton

    .word PB_name_6F9E6620
    .byte BasicMenu::ItemType::Button
    .word Practice_DoButton

    .word PB_name_9BC1471A
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
    dec BasicMenu::Selection
    lda BasicMenu::Selection
    asl a
    asl a
    tax
    lda PracticeBoards+2, x
    sta AddressPointer4+0
    lda PracticeBoards+3, x
    sta AddressPointer4+1

    lda #MMSel::Marathon
    sta ModeSelection

    lda #InitIndex::Game
    jmp GotoInit

.include "practice-boards.i"
