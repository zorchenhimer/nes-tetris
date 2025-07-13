.pushseg
.segment "PAGE_UTIL"

ReadControllers:
    lda Controller+0
    sta Controller_Old+0

    lda Controller+1
    sta Controller_Old+1

    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    LDX #$08
@player1:
    lda $4016
    lsr A           ; Bit0 -> Carry
    rol Controller+0 ; Bit0 <- Carry
    and #$01
    ora Controller+0
    sta Controller+0
    dex
    bne @player1

    ldx #1
    ldy #8
@player2:
    lda $4017
    lsr A              ; Bit0 -> Carry
    rol Controller+1 ; Bit0 <- Carry
    and #$01
    ora Controller+1
    sta Controller+1
    dey
    bne @player2

    rts

; Was a button pressed this frame?
; PlayerID in Y
ButtonPressed:
    sta TmpX
    and Controller, y
    sta TmpY

    lda Controller_Old, y
    and TmpX

    cmp TmpY
    bne @btnPress_stb

    ; no button change
    rts

@btnPress_stb:
    ; button released
    lda TmpY
    bne @btnPress_stc
    rts

@btnPress_stc:
    ; button pressed
    lda #1
    rts

; PlayerID in Y
ButtonReleased:
    sta TmpY
    and Controller_Old, y
    bne :+
    ; wasn't pressed last frame, can't be
    ; released this frame
    rts
:
    sta TmpX
    lda Controller, y
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
    bne :-
    rts

; Palette data address in AddressPointer1
; Loads up all four SP palettes.
LoadSpPalettes:
    ldy #0
    ldx #4
:
    lda (AddressPointer1), y
    sta Palettes+16, y
    iny
    lda (AddressPointer1), y
    sta Palettes+16, y
    iny
    lda (AddressPointer1), y
    sta Palettes+16, y
    iny
    lda (AddressPointer1), y
    sta Palettes+16, y
    iny
    dex
    bne :-
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
    sta SpriteStart, x
    inx
    inx
    inx
    inx
    bne @loop
    rts

; Run Length Encoded tile data at AddressPointer1
; PPU Hi Address A
DrawScreen_RLE:
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

    txa
    ldx #64
:   sta $2007
    dex
    bne :-
    rts

; Fill value in X
; PPU Hi Address A
FillScreen:
    sta $2006
    lda #$00
    sta $2006

    tya
    pha

    txa
    ldy #32
@outer:
    ldx #30
@inner:
    sta $2007
    dex
    bne @inner
    dey
    bne @outer

    pla
    tay
    rts

WaitForNMI:
    lda NmiHandler+1
    bne :+
    lda #.lobyte(BareNmiHandler)
    sta NmiHandler+0
    lda #.hibyte(BareNmiHandler)
    sta NmiHandler+1
:

    lda PpuControl
    ora #$80
    sta $2000

    lda #0
    sta Sleeping
:
    inc rng_index
    bit Sleeping
    bpl :-
    rts

WaitForIRQ:
    cli
    lda #0
    sta SleepingIrq
    sta Sleeping
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

BinToDec_Shift:
    lda #0
    .repeat .sizeof(bcdScratch), i
        sta bcdScratch+i
    .endrepeat

    .repeat 23
    clc
    rol bcdInput+0
    rol bcdInput+1
    rol bcdInput+2
    rol bcdScratch+0
    rol bcdScratch+1
    rol bcdScratch+2
    rol bcdScratch+3

    ; One
    lda bcdScratch+0
    and #$0F
    cmp #$05
    bcc :+
    clc
    lda bcdScratch+0
    adc #$03
    sta bcdScratch+0
:

    ; Ten
    lda bcdScratch+0
    and #$F0
    cmp #$50
    bcc :+
    clc
    lda bcdScratch+0
    adc #$30
    sta bcdScratch+0
:

    ; Hundred
    lda bcdScratch+1
    and #$0F
    cmp #$05
    bcc :+
    clc
    lda bcdScratch+1
    adc #$03
    sta bcdScratch+1
:

    ; 1k
    lda bcdScratch+1
    and #$F0
    cmp #$50
    bcc :+
    clc
    lda bcdScratch+1
    adc #$30
    sta bcdScratch+1
:

    ; 10k
    lda bcdScratch+2
    and #$0F
    cmp #$05
    bcc :+
    clc
    lda bcdScratch+2
    adc #$03
    sta bcdScratch+2
:

    ; 100k
    lda bcdScratch+2
    and #$F0
    cmp #$50
    bcc :+
    clc
    lda bcdScratch+2
    adc #$30
    sta bcdScratch+2
:

    ; 1mil
    lda bcdScratch+3
    and #$0F
    cmp #$05
    bcc :+
    clc
    lda bcdScratch+3
    adc #$03
    sta bcdScratch+3
:

    ; 10mil
    lda bcdScratch+3
    and #$F0
    cmp #$50
    bcc :+
    clc
:
    .endrepeat

    clc
    rol bcdInput+0
    rol bcdInput+1
    rol bcdInput+2
    rol bcdScratch+0
    rol bcdScratch+1
    rol bcdScratch+2
    rol bcdScratch+3

    ; make it ascii

    lda bcdScratch+3
    lsr a
    lsr a
    lsr a
    lsr a
    ora #$30
    sta bcdOutput+0

    lda bcdScratch+3
    and #$0F
    ora #$30
    sta bcdOutput+1

    ;;;
    lda bcdScratch+2
    lsr a
    lsr a
    lsr a
    lsr a
    ora #$30
    sta bcdOutput+2

    lda bcdScratch+2
    and #$0F
    ora #$30
    sta bcdOutput+3

    ;;;
    lda bcdScratch+1
    lsr a
    lsr a
    lsr a
    lsr a
    ora #$30
    sta bcdOutput+4

    lda bcdScratch+1
    and #$0F
    ora #$30
    sta bcdOutput+5

    ;;;
    lda bcdScratch+0
    lsr a
    lsr a
    lsr a
    lsr a
    ora #$30
    sta bcdOutput+6

    lda bcdScratch+0
    and #$0F
    ora #$30
    sta bcdOutput+7

    ldx #0
    ldy #' '
:
    lda bcdOutput, x
    cmp #$30
    bne :+
    sty bcdOutput, x
    inx
    cpx #7
    bne :-
:

    rts

; AddressPointer1 - source
; AddressPointer2 - destination
; Y - length
MemCopyRev:
    dey
@loop:
    lda (AddressPointer1), y
    sta (AddressPointer2), y
    dey
    bne @loop
    lda (AddressPointer1), y
    sta (AddressPointer2), y
    rts

Init_Routines:
    ;.word InitGame
    .word StartGame
    .word InitScores
    .word InitModeMenu
    .word InitVsMode
    .word InitOptions
    .word InitScores_EnterName
    .word InitMenu
    .word InitPracticeMenu
    .word InitConfirmClear

Init_ScreensNT0:
    .word Screen_Playfield
    .word Screen_Scores
    .word Screen_ModeMenu
    .word Screen_PlayfieldVs ; vs
    .word $0000 ; options
    .word Screen_NewHighScore
    .word Screen_Menu
    .word $0000 ; practice menu
    .word $0000 ; confirm clear

Init_ScreensNT1:
    .word $0000 ; game
    .word Screen_Scores
    .word $0000 ; modes
    .word $0000 ; vs
    .word $0000 ; options
    .word Screen_NewHighScore_Shifted ; new score
    .word $0000 ; menu
    .word $0000 ; practice menu
    .word $0000 ; confirm clear

Init_BanksLower: ; lower 16k
    .byte 0 ; game
    .byte 1 ; Scores
    .byte 1 ; ModeMenu
    .byte 0 ; VsMode
    .byte 1 ; options
    .byte 1 ; Scores_EnterName
    .byte 1 ; Main Menu
    .byte 1 ; practice menu
    .byte 1 ; confirm clear

; Destination index in A
GotoInit:
    pha ; index
    asl a
    pha ; offset

    DisableIRQ
    DisableRam
    ResetNMI

    lda #0
    sta PpuMask

    jsr WaitForNMI

    ; do some common stuff
    jsr WaitForNMI
    jsr ClearSprites

    lda #0
    sta $2000

    ; send cleared sprites
    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #$00
    sta ScrollX
    sta ScrollY

    ldx #0
    jsr FillAttributeTable
    jsr ClearExtAttr

    lda #1 ; screen bank
    jsr MMC5_SelectLowBank

    pla
    tax
    pha
    lda Init_ScreensNT0+1, x
    beq @clearNt0
    sta AddressPointer1+1
    lda Init_ScreensNT0+0, x
    sta AddressPointer1+0
    lda #$20
    jsr DrawScreen_RLE
    jmp :+
@clearNt0:
    ldx #' '
    lda #$20
    jsr FillScreen
:

    pla
    tax
    pha
    lda Init_ScreensNT1+1, x
    beq @clearNt1
    sta AddressPointer1+1
    lda Init_ScreensNT1+0, x
    sta AddressPointer1+0
    lda #$24
    jsr DrawScreen_RLE
    jmp :+
@clearNt1:
    ldx #' '
    lda #$24
    jsr FillScreen
:

    pla
    tax
    lda Init_Routines+0, x
    sta AddressPointer1+0
    lda Init_Routines+1, x
    sta AddressPointer1+1

    pla
    tax
    lda Init_BanksLower, x
    jsr MMC5_SelectLowBank

    ldx #$FF
    txs

    jmp (AddressPointer1)
    ;rts

.popseg
