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

; Palette data address in AddressPointer1
; Loads up all four BG palettes.
LoadBgPalettes:
    ldy #0
    ldx #4
:
    lda (AddressPointer1), y
    sta Palettes, y
    iny
    lda (AddressPointer1), y
    sta Palettes, y
    iny
    lda (AddressPointer1), y
    sta Palettes, y
    iny
    lda (AddressPointer1), y
    sta Palettes, y
    iny
    dex
    bpl :-
    rts

; Binary value in A, decimal values
; output in BinOutput
; TODO: Split this in two?  one for six
;       characters (this one), and one for
;       four?
BinToDec:
    lda #'0'
    .repeat .sizeof(Bin_Tiles), i
    sta Bin_Tiles+i
    .endrepeat

    ; binary to ascii
    lda #4
    sta TmpX ; digit index
    ldx #0   ; ASCII index
@binLoop:
    ldy TmpX ; get index into DecimalPlaces
    lda Mult3, y

    tay
    lda Bin_Input+2
    cmp DecimalPlaces+2, y
    bcc @binNext
    bne @binSub

    lda Bin_Input+1
    cmp DecimalPlaces+1, y
    bcc @binNext
    bne @binSub

    lda Bin_Input+0
    cmp DecimalPlaces+0, y
    bcc @binNext

@binSub:
    ; the subtractions
    sec
    .repeat .sizeof(Bin_Input), i
    lda Bin_Input+i
    sbc DecimalPlaces+i, y
    sta Bin_Input+i
    .endrepeat
    inc Bin_Tiles, x
    jmp @binLoop

@binNext:
    ; next digit
    inx
    dec TmpX
    bpl @binLoop

    ; one's place
    lda Bin_Input+0
    ora #'0'
    sta Bin_Tiles+5
    rts

DecimalPlaces:
    .byte $0A, $00, $00 ; 10
    .byte $64, $00, $00 ; 100
    .byte $E8, $03, $00 ; 1000
    .byte $10, $27, $00 ; 10000
    .byte $A0, $86, $01 ; 100000

Mult3:
    .repeat 5, i
    .byte (i)*3
    .endrepeat

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

    ldx #240
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
    lda #0
    sta Sleeping
:
    inc rng_index
    bit Sleeping
    bpl :-
    rts

