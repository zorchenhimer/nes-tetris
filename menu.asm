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

MenuClearFn:  .res 2
MenuSelectFn: .res 2

.popseg

.enum MenuSel
Game
Scores
Modes
VsMode
Options
.endenum

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

;NmiMenu:
;    rts

MenuPalettes:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word :+++++

:   .byte $0F, $2A, $0A, $1A
:   .byte $0F, $21, $01, $11
:   .byte $0F, $27, $07, $17
:   .byte $0F, $24, $04, $14
:   .byte $0F, $2C, $0C, $1C

MenuOutlinePal:
    .byte $0F, $00, $20, $10

IrqMenu:
    lda MenuSelectFn+1
    bne :+
    rts
:
    jsr @clear
    jsr @select

    lda #0
    sta MenuSelectFn+1
    rts

@clear:
    lda #0
    ldx #$00
    ldy #$00
    jmp (MenuClearFn)

@select:
    lda #$40
    ldx #$01
    ldy #$02
    jmp (MenuSelectFn)

MenuIrqFunctions:
    .word menu_Game
    .word menu_Scores
    .word menu_Modes
    .word menu_VsMode
    .word menu_Options

menu_Game:
    ; The box, horiz
    .repeat 9, i
        stx $2082+MMC5_OFFSET+i
        stx $2202+MMC5_OFFSET+i
    .endrepeat

    ; The box, vert
    .repeat 11, i
        stx $20A2+MMC5_OFFSET+(i*32)
        sty $20AA+MMC5_OFFSET+(i*32)
    .endrepeat

    ; Start
    .repeat 5, i
        sta $20C4+MMC5_OFFSET+i
    .endrepeat

    ; Top 2/3 of I piece
    sta $2106+MMC5_OFFSET
    sta $2126+MMC5_OFFSET

    ; Rest of playfield
    .repeat 6, j
        .repeat 5, i
            sta $2144+MMC5_OFFSET+i+(j*32)
        .endrepeat
    .endrepeat
    rts

menu_Scores:
    ; Box, horiz
    .repeat 10, i
        stx $208B+MMC5_OFFSET+i
        stx $220B+MMC5_OFFSET+i
    .endrepeat

    ; The box, vert
    .repeat 11, i
        stx $20AA+MMC5_OFFSET+(i*32)
        sty $20B5+MMC5_OFFSET+(i*32)
    .endrepeat

    sty $208A+MMC5_OFFSET
    sty $220A+MMC5_OFFSET

    stx $2095+MMC5_OFFSET
    stx $2215+MMC5_OFFSET

    ; HIGH
    .repeat 4, i
        sta $20CE+MMC5_OFFSET+i
    .endrepeat

    ; SCORES
    .repeat 6, i
        sta $20ED+MMC5_OFFSET+i
    .endrepeat

    ; Score List
    .repeat 4, j
        .repeat 8, i
            sta $214D+MMC5_OFFSET+i+(j*32)
        .endrepeat
    .endrepeat

    ; The numbers are using standard ASCII, but it
    ; needs to be from different banks to get the
    ; colors correct (ie, different from the
    ; selection title).
    cmp #$40
    bne @select
    lda #%0100_0010
    jmp :+
@select:
    lda #%0000_0010
:
    .repeat 4, j
        sta $214C+MMC5_OFFSET+(j*32)
    .endrepeat
    rts

menu_Modes:
    ; Box, horiz
    .repeat 8, i
        stx $2096+MMC5_OFFSET+i
        stx $2216+MMC5_OFFSET+i
    .endrepeat

    .repeat 11, i
        stx $20B5+MMC5_OFFSET+(i*32)
        stx $20BD+MMC5_OFFSET+(i*32)
    .endrepeat

    sty $2095+MMC5_OFFSET
    sty $2215+MMC5_OFFSET

    ; Game
    .repeat 4, i
        sta $20D7+MMC5_OFFSET+i
    .endrepeat

    ; Modes
    .repeat 5, i
        sta $20F7+MMC5_OFFSET+i
    .endrepeat

    ; ???
    .repeat 3, i
        sta $2138+MMC5_OFFSET+i
    .endrepeat

    ; Playfield
    .repeat 6, j
        .repeat 5, i
            sta $2157+MMC5_OFFSET+i+(j*32)
        .endrepeat
    .endrepeat
    rts

menu_VsMode:
    ; Box, horiz
    .repeat 14, i
        stx $2222+MMC5_OFFSET+i
        stx $2302+MMC5_OFFSET+i
    .endrepeat

    ; Box, vert
    .repeat 6, i
        stx $2242+MMC5_OFFSET+(i*32)
        stx $224F+MMC5_OFFSET+(i*32)
    .endrepeat

    ; Two
    .repeat 3, i
        sta $2264+MMC5_OFFSET+i
    .endrepeat

    ; Player
    .repeat 6, i
        sta $2268+MMC5_OFFSET+i
    .endrepeat

    ; Controllers
    .repeat 2, j
        .repeat 10, i
            sta $22A4+MMC5_OFFSET+i+(j*32)
        .endrepeat
    .endrepeat
    rts

menu_Options:
    ; Box, horiz
    .repeat 14, i
        stx $2230+MMC5_OFFSET+i
        stx $2310+MMC5_OFFSET+i
    .endrepeat

    ; Box, vert
    .repeat 6, i
        stx $2250+MMC5_OFFSET+(i*32)
        stx $225D+MMC5_OFFSET+(i*32)
    .endrepeat

    ; Options
    .repeat 7, i
        sta $2273+MMC5_OFFSET+i
    .endrepeat

    ; Icons
    .repeat 2, j
        .repeat 8, i
            sta $22B3+MMC5_OFFSET+i+(j*32)
        .endrepeat
    .endrepeat
    rts


InitMenu:
    lda #.lobyte(Palette_Bg)
    sta AddressPointer1+0
    lda #.hibyte(Palette_Bg)
    sta AddressPointer1+1

    ldx #0
    jsr LoadPalette

    ldx #4
    jsr LoadPalette

    lda MenuPalettes+0
    sta AddressPointer1+0
    lda MenuPalettes+1
    sta AddressPointer1+1

    ldx #1
    jsr LoadPalette

    lda #.hibyte(Menu_StartAddr)
    sta AddressPointer1+1
    sta $2006
    lda #.lobyte(Menu_StartAddr)
    sta AddressPointer1+0
    sta $2006

    lda #0
    sta MenuSelection

    lda #.lobyte(menu_Game)
    sta MenuSelectFn+0
    lda #.hibyte(menu_Game)
    sta MenuSelectFn+1

    lda #.lobyte(menu_Scores)
    sta MenuClearFn+0
    lda #.hibyte(menu_Scores)
    sta MenuClearFn+1

    ; Turn ExtAttr mode back on
    lda #%0000_0001
    sta $5104

    lda #%1000_0000
    sta PpuControl
    sta $2000

    lda #0
    sta ScrollX
    sta ScrollY

    lda #%0000_1110
    sta PpuMask

    jsr WaitForNMI

    SetIRQ 5, IrqMenu
    jsr WaitForNMI

FrameMenu:
    jsr ReadControllers

    lda #BUTTON_START ; start
    jsr ButtonPressed
    beq :+

    lda #MMSel::Marathon ; MMSel::Marathon
    sta ModeSelection

    lda MenuSelection
    jmp GotoInit

:

    lda #BUTTON_A ; a
    jsr ButtonPressed
    beq :+

    lda #MMSel::Marathon ; MMSel::Marathon
    sta ModeSelection

    lda MenuSelection
    jmp GotoInit
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
    lda #MenuDir::Up
    sta MenuSelDir
:

    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    lda #MenuDir::Down
    sta MenuSelDir
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :+
    lda #MenuDir::Left
    sta MenuSelDir
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    lda #MenuDir::Right
    sta MenuSelDir
:

    bit MenuSelDir
    bmi @noSelection

    lda MenuSelection
    sta MenuPrevious
    asl a
    tax
    asl a
    pha
    lda MenuIrqFunctions+0, x
    sta MenuClearFn+0
    lda MenuIrqFunctions+1, x
    sta MenuClearFn+1

    pla
    clc
    adc MenuSelDir
    tax
    lda MenuMovement, x
    sta MenuSelection

    asl a
    tax
    lda MenuIrqFunctions+0, x
    sta MenuSelectFn+0
    lda MenuIrqFunctions+1, x
    sta MenuSelectFn+1

    lda MenuPalettes+0, x
    sta AddressPointer1+0
    lda MenuPalettes+1, x
    sta AddressPointer1+1

    ldx #1
    jsr LoadPalette

    lda #3
    ldx #0
    jsr fs_Sfx_Play

@noSelection:

    jsr WaitForIRQ
    jmp FrameMenu
