.pushseg
.segment "BSS"
SingleBlockId: .res 1
ModeSelection: .res 1
TimeModeType:  .res 1

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
    .byte MMSel::Marathon,    MMSel::SingleBlock, MMSel::Marathon,    MMSel::Classic     ; Marathon
    .byte MMSel::Classic,     MMSel::SingleBlock, MMSel::Marathon,    MMSel::Classic     ; Classic
    .byte MMSel::Marathon,    MMSel::TimeAttack,  MMSel::SingleBlock, MMSel::SingleBlock ; Single Block
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

    ;lda #.lobyte(mm_Marathon)
    ;sta MenuSelectFn+0
    ;sta MenuClearFn+0
    ;lda #.hibyte(mm_Marathon)
    ;sta MenuSelectFn+1
    ;sta MenuClearFn+1

    ; Turn ExtAttr mode off
    lda #%0000_0010
    sta $5104

    lda ModeSelection
    asl a
    tax
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

    lda #%1000_1000
    sta PpuControl

    SetIRQ 5, IrqModeMenu

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

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
    beq :++

    lda ModeSelection
    cmp #MMSel::SingleBlock
    bne :+
    dec SingleBlockId
    bpl :++
    lda #4
    sta SingleBlockId
    jmp :++
:
    lda #MenuDir::Left
    sta MenuSelDir
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :++

    lda ModeSelection
    cmp #MMSel::SingleBlock
    bne :+
    inc SingleBlockId
    lda SingleBlockId
    cmp #5
    bne :++
    lda #0
    sta SingleBlockId
    jmp :++
:
    lda #MenuDir::Right
    sta MenuSelDir
:

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

    jsr UpdateSingleBlock

    jsr WaitForNMI
    jmp FrameModeMenu

UpdateSingleBlock:
    lda ModeSelection
    cmp #MMSel::SingleBlock
    beq :+
    lda #.lobyte(ModeSpritePalettes_Dark)
    sta AddressPointer1+0
    lda #.hibyte(ModeSpritePalettes_Dark)
    sta AddressPointer1+1
    jmp :++
:
    lda #.lobyte(ModeSpritePalettes_Bright)
    sta AddressPointer1+0
    lda #.hibyte(ModeSpritePalettes_Bright)
    sta AddressPointer1+1

:
    jsr LoadSpPalettes

    ldx SingleBlockId
    lda SingleBlockTiles, x
    sta $5127

    lda SingleBlockPals, x
    sta TmpY
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
    sta SpriteBlock+3, y

    lda SingleBlocks+0, x ; Y
    asl a
    asl a
    asl a
    clc
    adc #79
    sta SpriteBlock+0, y

    lda #0
    sta SpriteBlock+1, y

    lda TmpY
    sta SpriteBlock+2, y

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
    .word mm_Marathon
    .word mm_Classic
    .word mm_SingleBlock
    .word mm_TimeAttack
    .word mm_NoHold
    .word mm_DirtyBoard

mm_Marathon:
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

