.pushseg
.segment "ZEROPAGE"

.segment "BSS"

Opt_Selection:     .res 1
Opt_PrevSelection: .res 1

Opt_NmiUpdate_Addr: .res 2
Opt_NmiUpdate_Data: .res 3
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
OptItems_Names:
    .word :+
    .word :++
    .word :+++
    .word :++++
    .word $0000

:   .asciiz "Enable ghost flash"
:   .asciiz "Enable screen shake"
:   .asciiz "Shift start delay"
:   .asciiz "Shift repeat delay"


OptItems_Types:
    .byte 0
    .byte 0
    .byte 1
    .byte 1
OptionItems_Len = (* - OptItems_Types)

OptItems_Pointers:
    .word Option_GhostFlash
    .word Option_ScreenShake
    .word Option_ShiftStart
    .word Option_ShiftRepeat
    .word $0000

InitOptions:
    DisableIRQ

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

    ldx #0
    jsr FillAttributeTable
    ldx #' '
    jsr FillScreen

    jsr ClearExtAttr

    lda #.hibyte(OptTitle_ADDR)
    sta $2006
    lda #.lobyte(OptTitle_ADDR)
    sta $2006

    ldx #0
:   lda OptTitle, x
    beq :+
    sta $2007
    inx
    jmp :-
:

    jsr ClearSprites

    lda #0
    sta TmpX
    sta Opt_Selection
    sta Opt_PrevSelection

    lda #.lobyte(OptItems_ADDR)
    sta AddressPointer3+0
    lda #.hibyte(OptItems_ADDR)
    sta AddressPointer3+1

@itemLoop:
    lda AddressPointer3+1
    sta $2006
    lda AddressPointer3+0
    sta $2006

    lda TmpX
    asl a
    tax
    lda OptItems_Pointers+1, x
    sta AddressPointer2+1
    beq @done
    lda OptItems_Pointers+0, x
    sta AddressPointer2+0
    ldy #0
    lda (AddressPointer2), y
    sta TmpA

    ldx TmpX
    lda OptItems_Types, x
    bne @val
    lda TmpA
    beq @off
    ldy #OptTile_ON
    jmp :+
@off:
    ldy #OptTile_OFF
:
    sty $2007
    iny
    sty $2007
    iny
    sty $2007
    lda #' '
    sta $2007
    jmp @txt
@val:
    lda TmpA
    jsr BinToDec_8bit
    .repeat 3, i
        lda bcdOutput+i
        sta $2007
    .endrepeat
    lda #' '
    sta $2007
    ;sta $2007
    ;sta $2007
    ;sta $2007

@txt:
    lda TmpX
    asl a
    tax
    lda OptItems_Names+1, x
    sta AddressPointer1+1
    lda OptItems_Names+0, x
    sta AddressPointer1+0
    inc TmpX

    ldy #0
:   lda (AddressPointer1), y
    beq :+
    sta $2007
    iny
    jmp :-
:

    clc
    lda AddressPointer3+0
    adc #32*2
    sta AddressPointer3+0

    lda AddressPointer3+1
    adc #0
    sta AddressPointer3+1
    jmp @itemLoop

@done:

    lda #0
    sta Opt_Selection
    sta Opt_NmiUpdate_Addr+0
    sta Opt_NmiUpdate_Addr+1

    lda #.lobyte(NmiOpt)
    sta NmiHandler+0
    lda #.hibyte(NmiOpt)
    sta NmiHandler+1

    lda #%0000_0001
    sta $5104

    lda #%1000_0000
    sta PpuControl
    sta $2000

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

FrameOptions:
    SetIRQ 2, irqOptions

    lda Opt_Selection
    sta Opt_PrevSelection

    jsr ReadControllers

    lda #BUTTON_DOWN ; down, pressed
    jsr ButtonPressed
    beq :+
    inc Opt_Selection
    cmp #OptionItems_Len
    bcc :+
    lda #0
    sta Opt_Selection
:

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    dec Opt_Selection
    bcc :+
    lda #OptionItems_Len-1
    sta Opt_Selection
:

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :+
    jsr Opt_DecVal
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    jsr Opt_IncVal
:

    lda #BUTTON_B ; b
    jsr ButtonPressed
    beq :+
    jsr WaitForNMI
    jmp InitMenu
:

    lda Opt_Selection
    pha
    asl a
    tax
    lda OptItems_Pointers+0, x
    sta AddressPointer1+0
    lda OptItems_Pointers+1, x
    sta AddressPointer1+1

    ldy #0
    lda (AddressPointer1), y
    sta TmpA

    pla
    tax
    lda OptItems_Types, x
    beq @bool
    lda TmpA
    jsr BinToDec_8bit
    .repeat 3, i
        lda bcdOutput+i
        sta Opt_NmiUpdate_Data+i
    .endrepeat
    jmp @done
@bool:
    lda TmpA
    beq @off
    ldy #OptTile_ON
    jmp :+
@off:
    ldy #OptTile_OFF
:
    .repeat 3, i
    sty Opt_NmiUpdate_Data+i
    iny
    .endrepeat
@done:

    lda Opt_Selection
    sta MMC5_MultA
    lda #32*2
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc #.lobyte(OptItems_ADDR)
    sta Opt_NmiUpdate_Addr+0

    lda MMC5_MultB
    adc #.hibyte(OptItems_ADDR)
    sta Opt_NmiUpdate_Addr+1

    jsr WaitForNMI
    jmp FrameOptions

Opt_IncVal:
    ldx Opt_Selection
    lda OptItems_Types, x
    beq opt_BoolVal
    lda #$01
    sta TmpA
    jmp opt_changeVal


Opt_DecVal:
    ldx Opt_Selection
    lda OptItems_Types, x
    beq opt_BoolVal
    lda #$FF
    sta TmpA

opt_changeVal:
    lda Opt_Selection
    asl a
    tax
    lda OptItems_Pointers+0, x
    sta AddressPointer1+0
    lda OptItems_Pointers+1, x
    sta AddressPointer1+1

    ldy #0
    clc
    lda (AddressPointer1), y
    adc TmpA
    sta (AddressPointer1), y
    rts

opt_BoolVal:
    lda Opt_Selection
    asl a
    tax
    lda OptItems_Pointers+0, x
    sta AddressPointer1+0
    lda OptItems_Pointers+1, x
    sta AddressPointer1+1

    ldy #0
    lda (AddressPointer1), y
    beq :+
    lda #0
    sta (AddressPointer1), y
    rts
:
    lda #1
    sta (AddressPointer1), y
    rts

irqOptions:
    lda Opt_PrevSelection
    ldx #$00
    jsr irqOpt_WriteAttr

    lda Opt_Selection
    ldx #$40
    jsr irqOpt_WriteAttr
    rts

; A is option
; X is fill val
irqOpt_WriteAttr:
    sta MMC5_MultA
    lda #32*2
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc #.lobyte(OptItems_ADDR+MMC5_OFFSET)
    sta AddressPointer1+0

    lda MMC5_MultB
    adc #.hibyte(OptItems_ADDR+MMC5_OFFSET)
    sta AddressPointer1+1

    ldy #0
    txa
    ldx #28
:   sta (AddressPointer1), y
    iny
    dex
    bne :-
    rts

NmiOpt:
    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    .repeat 4*8, i
        lda Palettes+i
        sta $2007
    .endrepeat

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda Opt_NmiUpdate_Addr+1
    bne :+
    rts
:

    sta $2006
    lda Opt_NmiUpdate_Addr+0
    sta $2006

    .repeat 3, i
    lda Opt_NmiUpdate_Data+i
    sta $2007
    .endrepeat

    lda #0
    sta ScrollX
    sta ScrollY
    rts
