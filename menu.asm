Menu_StartAddr   = $210C

Menu_ItemSpacing = 2 ; lines-ish

Menu_ItemSpacing_Tiles  = Menu_ItemSpacing * 32
Menu_ItemSpacing_Cursor = Menu_ItemSpacing * 8

Menu_CursorTile = '>'
Menu_CursorX    = 85
Menu_CursorY    = 63 ; First row

.pushseg
.segment "BSS"
MenuSelection: .res 1
MenuPrevious:  .res 1
MenuSelDir:    .res 1
UpdateMenu:    .res 1

.popseg

MenuText:
    .asciiz "Start"
    .asciiz "High Scores"
    .asciiz "High Scores"
    .asciiz "High Scores"

MenuDestinations:
    .word InitGame
    .word InitScores
    .word InitModes
    .word InitVsMode
    .word InitOptions

MenuDestCount = (* - MenuDestinations) / 2

.enum MenuSel
Game
Scores
Modes
VsMode
Options
.endenum

;InitScores:
;    jmp InitScores

InitModes:
    jmp InitModes

InitVsMode:
    jmp InitVsMode

InitOptions:
    jmp InitOptions

.enum MenuDir
Up
Down
Left
Right
.endenum

MenuMovement:
    ; U D L R
    .byte MenuSel::Game,   MenuSel::VsMode,  MenuSel::Game,   MenuSel::Scores
    .byte MenuSel::Scores, MenuSel::VsMode,  MenuSel::Game,   MenuSel::Modes
    .byte MenuSel::Modes,  MenuSel::Options, MenuSel::Scores, MenuSel::Modes

    .byte MenuSel::Game,   MenuSel::VsMode,  MenuSel::VsMode, MenuSel::Options
    .byte MenuSel::Modes,  MenuSel::Options, MenuSel::VsMode, MenuSel::Options


Menu_ItemCount = (* - MenuDestinations) / 2

;NmiMenu:
;    rts

InitMenu:
    lda #%0000_0000
    sta $5104

    jsr DisableIrq

    lda #.lobyte(BareNmiHandler)
    sta NmiHandler+0
    lda #.hibyte(BareNmiHandler)
    sta NmiHandler+1

    jsr ClearSprites

    ldx #0
    jsr FillAttributeTable

    lda #.lobyte(Screen_Menu)
    sta AddressPointer1+0
    lda #.hibyte(Screen_Menu)
    sta AddressPointer1+1
    jsr DrawScreen
    ;ldx #' '
    ;jsr FillScreen

    lda #.lobyte(Palette_Bg)
    sta AddressPointer1+0
    lda #.hibyte(Palette_Bg)
    sta AddressPointer1+1

    ldx #0
    jsr LoadPalette

    ldx #4
    jsr LoadPalette

    lda #.hibyte(Menu_StartAddr)
    sta AddressPointer1+1
    sta $2006
    lda #.lobyte(Menu_StartAddr)
    sta AddressPointer1+0
    sta $2006

    lda #0
    sta MenuSelection

    lda #%1000_0000
    sta PpuControl
    sta $2000

    lda #%0001_1110
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

FrameMenu:
    jsr ReadControllers

    lda #BUTTON_START ; start
    jsr ButtonPressed
    beq :+
    ; NOTE: this is just until i get the new menu working
    lda #0
    sta MenuSelection
    jmp Menu_DoSelection
:

    lda #BUTTON_A ; a
    jsr ButtonPressed
    beq :+
    ; NOTE: this is just until i get the new menu working
    lda #0
    sta MenuSelection
    jmp Menu_DoSelection
:

;    lda #BUTTON_SELECT ; select
;    jsr ButtonPressed
;    beq :+
;    inc MenuSelection
;:

    lda #$FF
    sta MenuSelDir

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    lda MenuDir::Up
    sta MenuSelDir
:

    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    lda MenuDir::Down
    sta MenuSelDir
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :+
    lda MenuDir::Left
    sta MenuSelDir
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    lda MenuDir::Right
    sta MenuSelDir
:

    bit MenuSelDir
    bmi @noSelection

    lda MenuSelection
    sta MenuPrevious
    asl a
    asl a
    clc
    adc MenuSelDir
    tax
    lda MenuMovement, x
    sta MenuSelection
    lda #1
    sta UpdateMenu

@noSelection:

;    ldx MenuSelection
;@loop:
;    beq @done
;    clc
;    lda SpriteZero+0
;    adc #Menu_ItemSpacing_Cursor
;    sta SpriteZero+0
;    dex
;    jmp @loop
;
;@done:

    jsr WaitForNMI
    jmp FrameMenu

Menu_DoSelection:
    lda MenuSelection
    asl a
    tax

    lda MenuDestinations+0, x
    sta AddressPointer1+0
    lda MenuDestinations+1, x
    sta AddressPointer1+1

    lda #0
    sta SpriteZero+0
    sta SpriteZero+1
    sta SpriteZero+2
    sta SpriteZero+3

    jsr WaitForNMI

    lda #0
    sta $2001

    lda #.lobyte(BareNmiHandler)
    sta NmiHandler+0
    lda #.hibyte(BareNmiHandler)
    sta NmiHandler+1

    jmp (AddressPointer1)

Screen_Menu:
    .include "menu-screen.i"
