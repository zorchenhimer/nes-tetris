.pushseg
.segment "BSS"
SingleBlockId: .res 1
ModeSelection: .res 1
TimeModeType:  .res 1

.segment "OAM"
TTSprites: .res 4*9

.popseg

ModeMenuPalettes:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word :+++++
    .word :++++++

:   .byte $0F, $2A, $0A, $1A
:   .byte $0F, $21, $01, $11
:   .byte $0F, $27, $07, $17
:   .byte $0F, $24, $14, $04
:   .byte $0F, $2C, $0C, $1C
:   .byte $0F, $2C, $0C, $1C

ModeSpritePalettes_Dark:
    .byte $0F, $00, $0A, $20
    .byte $0F, $13, $11, $20
    .byte $0F, $1C, $18, $20
    .byte $0F, $05, $17, $20

ModeSpritePalettes_Bright:
    .byte $0F, $00, $1A, $20
    .byte $0F, $23, $21, $20
    .byte $0F, $2C, $28, $20
    .byte $0F, $15, $27, $20

ModeMenuMovement:
    ;     Up                  Down                   Left                Right
    .byte MMSel::Practice,    MMSel::SingleBlock, MMSel::Practice,    MMSel::Classic     ; Practice
    .byte MMSel::Classic,     MMSel::SingleBlock, MMSel::Practice,    MMSel::Classic     ; Classic
    .byte MMSel::Practice,    MMSel::TimeAttack,  MMSel::SingleBlock, MMSel::SingleBlock ; Single Block
    .byte MMSel::SingleBlock, MMSel::NoHold,      MMSel::TimeAttack,  MMSel::TimeAttack  ; Time Attack
    .byte MMSel::TimeAttack,  MMSel::NoHold,      MMSel::NoHold,      MMSel::DirtyBoard  ; No Hold
    .byte MMSel::TimeAttack,  MMSel::DirtyBoard,  MMSel::NoHold,      MMSel::DirtyBoard  ; Dirty Board

InitModeMenu:
    lda ModeMenuPalettes+0
    sta AddressPointer1+0
    lda ModeMenuPalettes+1
    sta AddressPointer1+1

    ldx #1
    jsr LoadPalette

    lda #.lobyte(ModeSpritePalettes_Dark)
    sta AddressPointer1+0
    lda #.hibyte(ModeSpritePalettes_Dark)
    sta AddressPointer1+1
    jsr LoadSpPalettes

    ; Turn ExtAttr mode off
    lda #%0000_0010
    sta $5104

    lda #0
    sta ModeSelection

    ldx #0
    lda ModeMenuIrqFunctions+0, x
    sta AddressPointer1+0
    lda ModeMenuIrqFunctions+1, x
    sta AddressPointer1+1
    jsr :+
    jmp :++
:
    lda #$40
    ldx #$01
    ldy #$41
    jmp (AddressPointer1)
:

    ; Turn ExtAttr mode back on
    lda #%0000_0001
    sta $5104

; Time Attack selection sprites
    ldx #0
    ldy #0
:
    lda TimeModeText_X, x
    sta TTSprites+3, y
    lda #128
    sta TTSprites+0, y
    lda #1
    sta TTSprites+2, y
    inx
    iny
    iny
    iny
    iny
    cpy #(4*4)
    bne :-

; second row
    ldx #0
    ;ldy #0
:
    lda TimeModeText_X, x
    sta TTSprites+3, y
    lda #128+8
    sta TTSprites+0, y
    lda #1
    sta TTSprites+2, y
    inx
    iny
    iny
    iny
    iny
    cpy #(4*4)+(4*5)
    bne :-

    lda #%1000_1000
    sta PpuControl

    lda #0
    sta ScrollX
    sta ScrollY

    lda #%0001_1110
    sta PpuMask

    jsr WaitForNMI

    SetIRQ 5, IrqModeMenu

FrameModeMenu:
    jsr ReadControllers

    lda #BUTTON_B ; B
    jsr ButtonPressed
    beq :+

    lda #InitIndex::Menu
    jmp GotoInit
:

    lda #$FF
    sta MenuSelDir

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    lda #3
    ldx #FAMISTUDIO_SFX_CH0
    jsr fs_Sfx_Play
    lda #MenuDir::Up
    sta MenuSelDir
:

    lda #BUTTON_DOWN ; down
    jsr ButtonPressed
    beq :+
    lda #3
    ldx #FAMISTUDIO_SFX_CH0
    jsr fs_Sfx_Play
    lda #MenuDir::Down
    sta MenuSelDir
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq @noLeft
    lda #3
    ldx #FAMISTUDIO_SFX_CH0
    jsr fs_Sfx_Play

    lda ModeSelection
    cmp #MMSel::SingleBlock
    bne :+
    dec SingleBlockId
    bpl @noLeft
    lda #4
    sta SingleBlockId
    jmp @noLeft
:
    cmp #MMSel::TimeAttack
    bne @moveLeft

    lda TimeModeType
    eor #$01
    sta TimeModeType
    jmp @noLeft
@moveLeft:
    lda #MenuDir::Left
    sta MenuSelDir
@noLeft:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq @noRight
    lda #3
    ldx #FAMISTUDIO_SFX_CH0
    jsr fs_Sfx_Play

    lda ModeSelection
    cmp #MMSel::SingleBlock
    bne :+
    inc SingleBlockId
    lda SingleBlockId
    cmp #5
    bne @noRight
    lda #0
    sta SingleBlockId
    jmp @noRight
:
    cmp #MMSel::TimeAttack
    bne @moveRight

    lda TimeModeType
    eor #$01
    sta TimeModeType
    jmp @noRight
@moveRight:
    lda #MenuDir::Right
    sta MenuSelDir
@noRight:

    bit MenuSelDir
    bmi @noSelection

    lda ModeSelection
    sta MenuPrevious
    asl a
    tax
    asl a
    pha
    lda ModeMenuIrqFunctions+0, x
    sta MenuClearFn+0
    lda ModeMenuIrqFunctions+1, x
    sta MenuClearFn+1

    pla
    clc
    adc MenuSelDir
    tax
    lda ModeMenuMovement, x
    sta ModeSelection

    asl a
    tax
    lda ModeMenuIrqFunctions+0, x
    sta MenuSelectFn+0
    lda ModeMenuIrqFunctions+1, x
    sta MenuSelectFn+1

    lda ModeMenuPalettes+0, x
    sta AddressPointer1+0
    lda ModeMenuPalettes+1, x
    sta AddressPointer1+1

    ldx #1
    jsr LoadPalette

@noSelection:

    lda #BUTTON_A ; a
    jsr ButtonPressed
    bne @doStart

    lda #BUTTON_START ; start
    jsr ButtonPressed
    bne @doStart

    jsr UpdateSingleBlock
    jsr UpdateTimeAttack

    jsr WaitForIRQ
    jmp FrameModeMenu

@doStart:
    lda ModeSelection
    cmp #MMSel::Classic
    bne :+
    ; TODO: set the correct shit for this
    lda #InitIndex::Game
    jmp GotoInit

:   cmp #MMSel::Practice
    bne :+
    lda #InitIndex::PracticeMenu
    jmp GotoInit

:   lda #InitIndex::Game
    jmp GotoInit

UpdateTimeAttack:
    lda ModeSelection
    cmp #MMSel::TimeAttack
    beq :+
    ; inactive
    lda #$10
    jmp :++
:
    ; active
    lda #$14
:

    sta Palettes+(1*4)+1+16

    lda TimeModeType
    sta MMC5_MultA
    lda #9
    sta MMC5_MultB
    ldx MMC5_MultA

    ldy #0
:
    lda TimeModeText, x
    sta TTSprites+1, y
    inx
    iny
    iny
    iny
    iny
    cpy #(9*4)
    bne :-
    rts

UpdateSingleBlock:
    lda ModeSelection
    cmp #MMSel::SingleBlock
    beq :+
    ; inactive

    lda #$12
    jmp :++
:
    ; active

    lda #$11
:

    .repeat 4, i
    sta SpriteP1+1+(i*4)
    .endrepeat

    ;ldx #4
    ;jsr LoadPalette

    ldx SingleBlockId
    lda BlockColors, x
    sta Palettes+(0*4)+1+16
    lda BlockColors_Ghost, x
    sta Palettes+(0*4)+2+16

    ;lda SingleBlockPals, x
    ;sta TmpY
    txa
    asl a
    asl a
    asl a
    tax

    ldy #0
@loop:
    lda SingleBlocks+1, x ; X
    asl a
    asl a
    asl a
    clc
    adc #160
    sta SpriteP1+3, y

    lda SingleBlocks+0, x ; Y
    asl a
    asl a
    asl a
    clc
    adc #79
    sta SpriteP1+0, y

    ;lda #0
    ;sta SpriteP1+1, y

    ;lda TmpY
    ;sta SpriteP1+2, y

    iny
    iny
    iny
    iny

    inx
    inx
    cpy #4*4
    bne @loop
    rts

; Sprite offsets
SingleBlocks:
    ;     Y  X
    ; Z
    .byte 0, 0
    .byte 0, 1
    .byte 1, 1
    .byte 1, 2

    ; S
    .byte 0, 1
    .byte 0, 2
    .byte 1, 0
    .byte 1, 1

    ; T
    .byte 0, 1
    .byte 1, 0
    .byte 1, 1
    .byte 1, 2

    ; L
    .byte 0, 2
    .byte 1, 0
    .byte 1, 1
    .byte 1, 2

    ; J
    .byte 0, 0
    .byte 1, 0
    .byte 1, 1
    .byte 1, 2

SingleBlockTiles:
    .byte TILE_1
    .byte TILE_2
    .byte TILE_1
    .byte TILE_2
    .byte TILE_2

SingleBlockPals:
    .byte PAL_D >> 6
    .byte PAL_A >> 6
    .byte PAL_B >> 6
    .byte PAL_D >> 6
    .byte PAL_B >> 6

IrqModeMenu:
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
    ldy #$41
    jmp (MenuSelectFn)

ModeMenuIrqFunctions:
    .word mm_Practice
    .word mm_Classic
    .word mm_SingleBlock
    .word mm_TimeAttack
    .word mm_NoHold
    .word mm_DirtyBoard

mm_Practice:
    .repeat 12, i
        stx $2064+MMC5_OFFSET+i
        stx $20E4+MMC5_OFFSET+i
    .endrepeat

    .repeat 3, i
        stx $2084+MMC5_OFFSET+(i*32)
        stx $208F+MMC5_OFFSET+(i*32)
    .endrepeat

    .repeat 8, i
        sta $20A6+MMC5_OFFSET+i
    .endrepeat
    rts

mm_Classic:
    .repeat 12, i
        stx $2071+MMC5_OFFSET+i
        stx $20F1+MMC5_OFFSET+i
    .endrepeat

    .repeat 3, i
        stx $2091+MMC5_OFFSET+(i*32)
        stx $209B+MMC5_OFFSET+(i*32)
    .endrepeat

    .repeat 7, i
        sta $20B3+MMC5_OFFSET+i
    .endrepeat
    rts

mm_SingleBlock:
    .repeat 24, i
        stx $2104+MMC5_OFFSET+i
        stx $21A4+MMC5_OFFSET+i
    .endrepeat

    .repeat 4, i
        stx $2124+MMC5_OFFSET+(i*32)
        stx $213B+MMC5_OFFSET+(i*32)
    .endrepeat

    .repeat 6, i
        sta $2146+MMC5_OFFSET+i
        sta $2166+MMC5_OFFSET+i
    .endrepeat
    rts

mm_TimeAttack:
    .repeat 24, i
        stx $21C4+MMC5_OFFSET+i
        stx $2264+MMC5_OFFSET+i
    .endrepeat

    .repeat 4, i
        stx $21E4+MMC5_OFFSET+(i*32)
        stx $21FB+MMC5_OFFSET+(i*32)
    .endrepeat

    .repeat 6, i
        sta $2206+MMC5_OFFSET+i
        sta $2226+MMC5_OFFSET+i
    .endrepeat

    .repeat 5, i
        sty $2213+MMC5_OFFSET+i
        sty $2233+MMC5_OFFSET+i
    .endrepeat
    rts

mm_NoHold:
    .repeat 10, i
        stx $2284+MMC5_OFFSET+i
        stx $2324+MMC5_OFFSET+i
    .endrepeat

    .repeat 4, i
        stx $22A4+MMC5_OFFSET+(i*32)
        stx $22AD+MMC5_OFFSET+(i*32)
    .endrepeat

    .repeat 4, i
        sta $22C7+MMC5_OFFSET+i
        sta $22E7+MMC5_OFFSET+i
    .endrepeat
    rts

mm_DirtyBoard:
    .repeat 11, i
        stx $2291+MMC5_OFFSET+i
        stx $2331+MMC5_OFFSET+i
    .endrepeat

    .repeat 4, i
        stx $22B1+MMC5_OFFSET+(i*32)
        stx $22BB+MMC5_OFFSET+(i*32)
    .endrepeat

    .repeat 5, i
        sta $22D4+MMC5_OFFSET+i
        sta $22F4+MMC5_OFFSET+i
    .endrepeat
    rts

TimeModeText:
    .byte "40  "
    .byte "LINES"

    .byte "200K"
    .byte "SCORE"

TimeModeText_X:
    .repeat 5, i
        .byte 152 + (i*8)
    .endrepeat

