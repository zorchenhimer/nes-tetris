.scope BasicMenu

TitleAddr = $206A
ItemsAddr = $20C4

Tile_ON  = $AB
Tile_OFF = $BB

.enum ItemType
Boolean ; set high bit for save ram?
Number
Button
.endenum

.struct BasicMenuData
Name     .addr ; Pointer to text data
Type     .byte ; type value
Argument .addr ; Pointer to argument (data or routine)
.endstruct

.pushseg
.segment "ZEROPAGE"

.segment "BSS"
Selection:     .res 1
PrevSelection: .res 1

NmiUpdate_Addr: .res 2
NmiUpdate_Data: .res 3

; Should these be pointers?
Palette_ON:  .res 4
Palette_OFF: .res 4

; Starts with null terminated title text.  After that is a
; series of BasicMenuData structures in a null-terminated
; list (ie, the last "element's" title pointer will be
; null).
; (this is a pointer to the data)
MenuData: .res 2

ItemCount: .res 1
ReturnRoutine: .res 2

.popseg

Init:
    lda #0
    sta Selection
    sta PrevSelection

    lda #.lobyte(Nmi)
    sta NmiHandler+0
    lda #.hibyte(Nmi)
    sta NmiHandler+1

    ; Draw title
    lda #.hibyte(TitleAddr)
    sta $2006
    lda #.lobyte(TitleAddr)
    sta $2006

    lda MenuData+0
    sta AddressPointer1+0
    lda MenuData+1
    sta AddressPointer1+1

    ldy #0
:   lda (AddressPointer1), y
    beq :+
    sta $2007
    iny
    jmp :-
:

    ; Advance pointer beyond title
    iny
    clc
    tya ; iny before this?
    adc AddressPointer1+0
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    ; Update the data list pointer backup
    sta MenuData+1
    lda AddressPointer1+0
    sta MenuData+0

    lda #.lobyte(ItemsAddr)
    sta AddressPointer4+0
    lda #.hibyte(ItemsAddr)
    sta AddressPointer4+1

    lda #0
    sta ItemCount
    sta ReturnRoutine+0
    sta ReturnRoutine+1

    ; Draw items
@itemLoop:
    ldy #BasicMenuData::Name ; BasicMenuData::Name
    iny ; high byte in pointer
    lda (AddressPointer1), y
    bne :+
    jmp @loopDone
:
    inc ItemCount

    lda ReturnRoutine+1
    bne :+
    ldy #BasicMenuData::Argument ; BasicMenuData::Argument
    lda (AddressPointer1), y
    sta ReturnRoutine+0
    iny
    lda (AddressPointer1), y
    sta ReturnRoutine+1
:

    lda AddressPointer4+1
    sta $2006
    lda AddressPointer4+0
    sta $2006

    ldy #BasicMenuData::Type ; BasicMenuData::Type
    lda (AddressPointer1), y
    cmp #ItemType::Boolean ; ItemType::Boolean
    bne @notBool

    ldy #BasicMenuData::Argument ; BasicMenuData::Argument
    lda (AddressPointer1), y
    sta AddressPointer2+0
    iny
    lda (AddressPointer1), y
    sta AddressPointer2+1

    ldy #0
    lda (AddressPointer2), y
    beq @off
    ldy #Tile_ON
    jmp :+
@off:
    ldy #Tile_OFF
:
    .repeat 3, i
        sty $2007
        .if i < 2
            iny
        .endif
    .endrepeat

    jmp @text

@notBool:
    cmp #ItemType::Number ; ItemType::Number
    bne @notNumber

    ldy #BasicMenuData::Argument
    lda (AddressPointer1), y
    sta AddressPointer2+0
    iny
    lda (AddressPointer1), y
    sta AddressPointer2+1

    ldy #0
    lda (AddressPointer2), y
    jsr BinToDec_8bit

    .repeat 3, i
        lda bcdOutput+i
        sta $2007
    .endrepeat

    jmp @text

@notNumber:   ; default to ItemType::Button
    lda #' '
    .repeat 3
    sta $2007
    .endrepeat

@text:
    lda #' ' ; single space between toggle and name
    sta $2007

    ldy #BasicMenuData::Name
    lda (AddressPointer1), y
    sta AddressPointer2+0
    iny
    lda (AddressPointer1), y
    sta AddressPointer2+1

    ldy #0
:   lda (AddressPointer2), y
    beq @next
    sta $2007
    iny
    jmp :-

@next:
    ; next menu item
    clc
    lda AddressPointer1+0
    adc #.sizeof(BasicMenuData)
    sta AddressPointer1+0
    lda AddressPointer1+1
    adc #0
    sta AddressPointer1+1

    ; next PPU row (skip one)
    clc
    lda AddressPointer4+0
    adc #32*2
    sta AddressPointer4+0

    lda AddressPointer4+1
    adc #0
    sta AddressPointer4+1
    jmp @itemLoop

@loopDone:

    lda #%0000_0001
    sta $5104

    lda #%1000_0000
    sta PpuControl
    sta $2000

    lda #%0001_1110
    sta PpuMask

    jsr WaitForNMI

    SetIRQ 2, Irq

Frame:
    ldy #Player1
    jsr ReadControllers

    lda Selection
    sta PrevSelection

    lda #BUTTON_DOWN ; down, pressed
    jsr ButtonPressed
    beq :+
    lda #3
    ldx #FAMISTUDIO_SFX_CH0
    jsr fs_Sfx_Play
    inc Selection
    lda Selection
    cmp ItemCount
    bcc :+
    lda #0
    sta Selection
:

    lda #BUTTON_UP ; up
    jsr ButtonPressed
    beq :+
    lda #3
    ldx #FAMISTUDIO_SFX_CH0
    jsr fs_Sfx_Play
    dec Selection
    lda Selection
    bpl :+
    ldx ItemCount
    dex
    stx Selection
:

    ; Put the current option in AddressPointer1
    lda Selection
    sta MMC5_MultA
    lda #.sizeof(BasicMenuData)
    sta MMC5_MultB

    clc
    lda MenuData+0
    adc MMC5_MultA
    sta AddressPointer1+0
    lda MenuData+1
    adc MMC5_MultB
    sta AddressPointer1+1

    ; Put the argument in AddressPointer2
    ldy #BasicMenuData::Argument
    lda (AddressPointer1), y
    sta AddressPointer2+0
    iny
    lda (AddressPointer1), y
    sta AddressPointer2+1

    ldy #Player1

    lda #BUTTON_LEFT ; left
    jsr ButtonPressed
    beq :+
    jsr DecVal
:

    lda #BUTTON_RIGHT ; right
    jsr ButtonPressed
    beq :+
    jsr IncVal
:

    lda #BUTTON_B ; b
    jsr ButtonPressed
    beq :+
    jmp (ReturnRoutine)
:

    lda #BUTTON_A ; a
    jsr ButtonPressed
    beq :+

    ldy #BasicMenuData::Type
    lda (AddressPointer1), y
    cmp #ItemType::Button
    bne :+

    jsr DoButton
:

    ldy #0
    lda (AddressPointer2), y
    sta TmpA

    ldy #BasicMenuData::Type
    lda (AddressPointer1), y
    cmp #ItemType::Button
    beq @updateButton

    cmp #ItemType::Boolean
    beq @bool

    ;cmp #ItemType::Number
    lda TmpA
    jsr BinToDec_8bit
    .repeat 3, i
        lda bcdOutput+i
        sta NmiUpdate_Data+i
    .endrepeat
    jmp @updateAddr

@bool:
    lda TmpA
    beq @off
    ldy #OptTile_ON
    jmp :+
@off:
    ldy #OptTile_OFF
:
    .repeat 3, i
    sty NmiUpdate_Data+i
    iny
    .endrepeat
    jmp @updateAddr

@updateButton:
    lda #0
    sta NmiUpdate_Addr+0
    sta NmiUpdate_Addr+1
    jmp @frameEnd

@updateAddr:
    lda Selection
    sta MMC5_MultA
    lda #32*2
    sta MMC5_MultB

    clc
    lda MMC5_MultA
    adc #.lobyte(OptItems_ADDR)
    sta NmiUpdate_Addr+0

    lda MMC5_MultB
    adc #.hibyte(OptItems_ADDR)
    sta NmiUpdate_Addr+1

@frameEnd:
    jsr WaitForIRQ
    jmp Frame

DoButton:
    jmp (AddressPointer2)

IncVal:
    lda #$01
    sta TmpA
    jmp changeVal

DecVal:
    lda #$FF
    sta TmpA

changeVal:

    lda Selection
    sta MMC5_MultA
    lda #.sizeof(BasicMenuData)
    sta MMC5_MultB

    clc
    lda MenuData+0
    adc MMC5_MultA
    sta AddressPointer1+0
    lda MenuData+1
    adc MMC5_MultB
    sta AddressPointer1+1

    ldy #BasicMenuData::Argument
    lda (AddressPointer1), y
    sta AddressPointer2+0
    iny
    lda (AddressPointer1), y
    sta AddressPointer2+1

    ldy #BasicMenuData::Type
    lda (AddressPointer1), y

    beq boolVal
    cmp #ItemType::Button
    bne :+
    rts
:

    ldy #0
    clc
    lda (AddressPointer2), y
    adc TmpA
    sta (AddressPointer2), y
    rts

boolVal:
    ldy #0
    lda (AddressPointer2), y
    beq :+
    lda #0
    sta (AddressPointer2), y
    rts
:
    lda #1
    sta (AddressPointer2), y
    rts

Irq:
    lda PrevSelection
    ldx #$00
    jsr irq_WriteAttr

    lda Selection
    ldx #$40
    jsr irq_WriteAttr
    rts

; A is option
; X is fill val
irq_WriteAttr:
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

Nmi:
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

    lda NmiUpdate_Addr+1
    beq @end

    sta $2006
    lda NmiUpdate_Addr+0
    sta $2006

    .repeat 3, i
    lda NmiUpdate_Data+i
    sta $2007
    .endrepeat

@end:
    lda #0
    sta ScrollX
    sta ScrollY
    rts

.endscope
