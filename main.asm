
.include "nes2header.inc"
nes2mapper 0
nes2prg 1 * 16 * 1024
nes2chr 1 * 8 * 1024
;nes2wram 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers
.feature addrsize

.segment "ZEROPAGE"
Sleeping: .res 1
AddressPointer1: .res 2
AddressPointer2: .res 2
AddressPointer3: .res 2
NmiHandler: .res 2

TmpA: .res 1
TmpB: .res 1
TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

BinOutput: .res 4
IsPaused: .res 1

Controller: .res 1
Controller_Old: .res 1

.segment "OAM"
SpriteZero: .res 4
SpriteBlock: .res 4*4

.segment "BSS"
Palettes: .res 4*8
BufferedBlock: .res 4*4

.segment "VECTORS0"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR0"
    .incbin "tiles.chr"

;.segment "CHRHEX"
;    .incbin "hex.chr"

.segment "CHR1"
    ;.incbin "pattern-a.chr"

.segment "PAGE0"

; Button Constants
BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

    .include "utils.asm"

IRQ:
    rti

BareNmiHandler:
    rts

NMI:
    pha
    txa
    pha
    tya
    pha

    lda #$FF
    sta Sleeping

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

    jsr NmiTrampoline

    lda #0
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    pla
    tay
    pla
    tax
    pla
    rti

NmiTrampoline:
    jmp (NmiHandler)

RESET:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

    lda #.lobyte(BareNmiHandler)
    sta NmiHandler+0
    lda #.hibyte(BareNmiHandler)
    sta NmiHandler+1

    ldx #$40
    stx $4017   ; Disable APU frame IRQ

    ldx #$FF
    txs         ; Setup new stack

    inx         ; Now X = 0

    stx $2000   ; disable NMI
    stx $2001   ; disable rendering
    stx $4010   ; disable DMC IRQs

:   ; First wait for VBlank to make sure PPU is ready.
    bit $2002   ; test this bit with ACC
    bpl :- ; Branch on result plus

:   ; Clear RAM
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x

    inx
    bne :-  ; loop if != 0

:   ; Second wait for vblank.  PPU is ready after this
    bit $2002
    bpl :-

    lda #$0F
    .repeat 4*8, i
    sta Palettes+i
    .endrepeat

    jmp InitMenu


StartFrame:

    jsr ReadControllers
    lda #BUTTON_START
    jsr ButtonPressed
    bne :+
    jsr WaitForNMI
    jmp StartFrame

:   jsr ClearSprites

    lda #0
    sta IsPaused

    jsr WaitForNMI
    lda #0
    sta $2005
    sta $2005
    lda #%1000_0000
    sta $2000


Palette_Bg:
    .byte $0F, $20, $00, $10

    .include "scores.asm"
    .include "menu.asm"
    .include "game.asm"

Screen_Playfield:
    .include "playfield.i"
