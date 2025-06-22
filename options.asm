.pushseg
.segment "ZEROPAGE"

.segment "BSS"

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
:   .asciiz "Clear Save Data"


Options_PrevMenu:
    DisableIRQ
    DisableRam
    lda #InitIndex::Menu
    jmp GotoInit

Options_ClearSave:
    DisableIRQ
    DisableRam
    lda #InitIndex::ConfirmClear
    jmp GotoInit

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

DataConfirmClear:
    .asciiz "Are you sure?"

    .word :+
    .byte BasicMenu::ItemType::Button
    .word @back

    .word :++
    .byte BasicMenu::ItemType::Button
    .word @clear

    .word $0000

:   .asciiz "NO"
:   .asciiz "YES"

@back:
    lda #InitIndex::Options
    jmp GotoInit

@clear:
    DisableIRQ
    jsr ClearRam

    jsr WaitForNMI

    lda #$22
    sta $2006
    lda #$49
    sta $2006

    ldx #0
:   lda @clearText, x
    beq :+
    sta $2007
    inx
    jmp :-
:

    lda #0
    sta $2005
    sta $2005
    jsr WaitForNMI

    ; Wait two seconds, or if A, B, or
    ; START are pressed
    lda #120
    sta TmpA
@wait:
    jsr ReadControllers

    lda #BUTTON_B ; b
    jsr ButtonPressed
    bne @end

    lda #BUTTON_A ; a
    jsr ButtonPressed
    bne @end

    lda #BUTTON_START ; start
    jsr ButtonPressed
    bne @end

    jsr WaitForNMI
    dec TmpA
    bne @wait

@end:
    lda #InitIndex::Options
    jmp GotoInit

@clearText:
    .asciiz "Save Data Cleared"

InitConfirmClear:
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

    lda #.lobyte(DataConfirmClear)
    sta BasicMenu::MenuData+0
    lda #.hibyte(DataConfirmClear)
    sta BasicMenu::MenuData+1

    jmp BasicMenu::Init
