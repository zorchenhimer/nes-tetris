ReadControllers:
    lda Controller
    sta Controller_Old

    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    LDX #$08
@player1:
    lda $4016
    lsr A           ; Bit0 -> Carry
    rol Controller ; Bit0 <- Carry
    dex
    bne @player1
    rts

; Was a button pressed this frame?
ButtonPressed:
    sta TmpX
    and Controller
    sta TmpY

    lda Controller_Old
    and TmpX

    cmp TmpY
    bne btnPress_stb

    ; no button change
    rts

btnPress_stb:
    ; button released
    lda TmpY
    bne btnPress_stc
    rts

btnPress_stc:
    ; button pressed
    lda #1
    rts

ButtonReleased:
    sta TmpY
    and Controller_Old
    bne :+
    ; wasn't pressed last frame, can't be
    ; released this frame
    rts
:
    sta TmpX
    lda Controller
    and TmpY
    eor TmpX
    rts

; Palette data address in AddressPointer1
; Palette index in X
LoadPalette:
    txa
    asl a
    asl a
    tax

    ldy #0
    lda (AddressPointer1), y
    sta Palettes+0, x
    iny
    lda (AddressPointer1), y
    sta Palettes+1, x
    iny
    lda (AddressPointer1), y
    sta Palettes+2, x
    iny
    lda (AddressPointer1), y
    sta Palettes+3, x

    rts

; Binary value in A, decimal values
; output in BinOutput
BinToDec:
    lda #0
    sta BinOutput+0
    sta BinOutput+1
    sta BinOutput+2
@high:
    lda TmpY
    beq @hundo

    sec
    lda TmpX
    sbc #100
    sta TmpX
    inc BinOutput+0
    bcs @high
    dec TmpY
    jmp @high

@hundo:
    lda TmpX
    cmp #100
    bcs @addhundo
    jmp @tens

@addhundo:
    inc BinOutput+0
    sec
    sbc #100
    sta TmpX
    jmp @hundo

@tens:
    lda TmpX
    cmp #10
    bcs @addtens
    jmp @done

@addtens:
    inc BinOutput+1
    sec
    sbc #10
    sta TmpX
    jmp @tens

@done:
    lda TmpX
    sta BinOutput+2
    rts

ClearSprites:
    ldx #0
    lda #$FF
@loop:
    sta SpriteZero, x
    inx
    inx
    inx
    inx
    bne @loop
    rts

; Tile data in AdressPointer1
DrawScreen:
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    ;lda #.lobyte(960)
    ;sta AddressPointer2+0
    ;lda #.hibyte(960)
    ;sta AddressPointer2+1
    ldx #240
    ;sta TmpA

@loop:
    ldy #0
    lda (AddressPointer1), y
    sta $2007
    iny

    lda (AddressPointer1), y
    sta $2007
    iny

    lda (AddressPointer1), y
    sta $2007
    iny

    lda (AddressPointer1), y
    sta $2007
    iny

    clc
    lda #4
    adc AddressPointer1+0
    sta AddressPointer1+0

    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    dex
    bne @loop
@done:
    rts

; Fill value in X
FillAttributeTable:

    lda #$23
    sta $2006
    lda #$C0
    sta $2006

    .repeat 64
    stx $2007
    .endrepeat
    rts

; Fill value in X
FillScreen:
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    .repeat 32
        .repeat 30
            stx $2007
        .endrepeat
    .endrepeat

    rts

WaitForNMI:
:   bit Sleeping
    bpl :-
    lda #0
    sta Sleeping
    rts
