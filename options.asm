.pushseg
.segment "ZEROPAGE"

.segment "BSS"

Opt_Selection:     .res 1
Opt_PrevSelection: .res 1

Opt_NmiUpdate_Addr: .res 2
Opt_NmiUpdate_Data: .res 3
.popseg

OptTitle_ADDR = $206A
OptTitle:
    .asciiz "Game Options"

OptPal_Off:
    .byte $0F, $20, $00, $10

OptPal_On:
    .byte $0F, $2A, $0A, $1A

OptTile_ON = $AB
OptTile_OFF = $BB

OptItems_ADDR = $20C4

:   .asciiz "<- Back"

OptionMenuData:
    .asciiz "Options"

    .word :-
    .byte BasicMenu::ItemType::Button
    .word Options_PrevMenu

    .word :+
    .byte BasicMenu::ItemType::Boolean
    .word Option_GhostFlash

    .word :++
    .byte BasicMenu::ItemType::Boolean
    .word Option_ScreenShake

    .word :+++
    .byte BasicMenu::ItemType::Number
    .word Option_ShiftStart

    .word :++++
    .byte BasicMenu::ItemType::Number
    .word Option_ShiftRepeat

    .word :+++++
    .byte BasicMenu::ItemType::Boolean
    .word Option_ShowNext

    .word :++++++
    .byte BasicMenu::ItemType::Boolean
    .word Option_ShowGhost

    .word :+++++++
    .byte BasicMenu::ItemType::Boolean
    .word Option_EnableHold

    .word :++++++++
    .byte BasicMenu::ItemType::Boolean
    .word Option_EnableHardDrop

    .word :+++++++++
    .byte BasicMenu::ItemType::Button
    .word Options_ClearSave

    .word $0000

:   .asciiz "Enable ghost flash"
:   .asciiz "Enable screen shake"
:   .asciiz "Shift start delay"
:   .asciiz "Shift repeat delay"
:   .asciiz "Dbg show next"
:   .asciiz "Dbg show ghost"
:   .asciiz "Dbg enable hold"
:   .asciiz "Dbg enable hard drop"
:   .asciiz "Dbg clear save"


Options_PrevMenu:
    DisableRam
    lda #InitIndex::Menu
    jmp GotoInit

Options_ClearSave:
    brk ; TODO
    rts

InitOptions:
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

    lda #.lobyte(OptionMenuData)
    sta BasicMenu::MenuData+0
    lda #.hibyte(OptionMenuData)
    sta BasicMenu::MenuData+1

    EnableRam
    jmp BasicMenu::Init

