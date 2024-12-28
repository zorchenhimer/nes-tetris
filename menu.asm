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

.popseg

MenuText:
    .asciiz "Start"
    .asciiz "High Scores"
    .asciiz "High Scores"
    .asciiz "High Scores"

MenuDestinations:
    .word InitGame
    .word InitScores
    .word InitScores
    .word InitScores

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

    ldx #' '
    jsr FillScreen

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

    ldx #0
    ldy #Menu_ItemCount
@loop:
    lda MenuText, x
    beq @next
    inx
    sta $2007
    jmp @loop

@next:
    inx
    dey
    beq @done
    clc
    lda AddressPointer1+0
    adc #Menu_ItemSpacing_Tiles
    sta AddressPointer1+0

    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
    sta $2006

    lda AddressPointer1+0
    sta $2006
    jmp @loop

@done:

    lda #0
    sta MenuSelection

    lda #Menu_CursorY
    sta SpriteZero+0
    lda #Menu_CursorX
    sta SpriteZero+3

    lda #Menu_CursorTile
    sta SpriteZero+1
    lda #0
    sta SpriteZero+2

    lda #%1000_0000
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
    jmp Menu_DoSelection
:

    lda #BUTTON_A ; a
    jsr ButtonPressed
    beq :+
    jmp Menu_DoSelection
:

    lda #BUTTON_SELECT ; select
    jsr ButtonPressed
    beq :+
    inc MenuSelection
:

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    dec MenuSelection
:

    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    inc MenuSelection
:

    ; Wrap around - top
    lda MenuSelection
    bpl :+
    ldx #Menu_ItemCount
    dex
    stx MenuSelection
:

    ; Wrap around - bottom
    lda MenuSelection
    cmp #Menu_ItemCount
    bcc :+
    lda #0
    sta MenuSelection
:

    lda #Menu_CursorY
    sta SpriteZero+0

    ldx MenuSelection
@loop:
    beq @done
    clc
    lda SpriteZero+0
    adc #Menu_ItemSpacing_Cursor
    sta SpriteZero+0
    dex
    jmp @loop

@done:

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
