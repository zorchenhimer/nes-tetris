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
    and #$01
    ora Controller
    sta Controller
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
BinToDec_Sm:
    lda #2
    sta TmpX
    ldx #2
    jmp binJmp

BinToDec:
    ; binary to ascii
    lda #4
    sta TmpX ; digit index
    ldx #0   ; ASCII index

binJmp:
    lda #'0'
    .repeat .sizeof(Bin_Tiles), i
    sta Bin_Tiles+i
    .endrepeat

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

; Run Length Encoded tile data at AddressPointer1
DrawScreen_RLE:
    lda #$20
    sta $2006
    lda #$00
    sta $2006

@top:
    ldy #0
    lda (AddressPointer1), y
    bne :+
    rts
:
    bmi @rle
    tax
    iny
@raw:
    lda (AddressPointer1), y
    sta $2007
    iny
    dex
    bne @raw
    jmp @next

@rle:
    and #$7F
    tax
    iny
    lda (AddressPointer1), y
:   sta $2007
    dex
    bne :-
    iny

@next:
    tya
    clc
    adc AddressPointer1+0
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1
    jmp @top
    ;rts

; Tile data in AddressPointer1
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

WaitForIRQ:
    lda #0
    sta SleepingIrq
:
    inc rng_index
    bit SleepingIrq
    bpl :-
    rts

; For use when drawing is off
ClearExtAttr:
    ; Turn off ExtAttr mode to draw the initial attribute data
    lda #%0000_0010
    sta $5104
    lda #0
    ldx #0
:
    sta $5C00+(0*256), x
    sta $5C00+(1*256), x
    sta $5C00+(2*256), x
    sta $5C00+(3*256), x
    dex
    bne :-

    ; Turn ExtAttr mode back on
    lda #%0000_0001
    sta $5104
    rts

BinToDec_8bit:
    tax
    lda @hundreds, x
    sta bcdOutput+0
    lda @tens, x
    sta bcdOutput+1
    lda @ones, x
    sta bcdOutput+2
    rts

@hundreds:
    .repeat 256, i
        .if i < 100
            .byte ' '
        .else
            .byte '0' + (i / 100)
        .endif
    .endrepeat

@tens:
    .repeat 256, i
        .if i < 10
            .byte ' '
        .else
            .byte '0' + ((i / 10) .mod 10)
        .endif
    .endrepeat

@ones:
    .repeat 256, i
        .byte '0' + (i .mod 10)
    .endrepeat
