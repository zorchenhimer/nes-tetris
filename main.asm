
.include "nes2header.inc"
nes2mapper 5
nes2prg 2 * 16 * 1024
nes2chr 2 * 8 * 1024
;nes2wram 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers
.feature addrsize

.segment "ZEROPAGE"
Sleeping: .res 1
SleepingIrq: .res 1
AddressPointer1: .res 2
AddressPointer2: .res 2
AddressPointer3: .res 2
NmiHandler: .res 2

TmpA: .res 1
TmpB: .res 1
TmpC: .res 1
TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

BinOutput: .res 4
IsPaused: .res 1

Controller: .res 1
Controller_Old: .res 1

ptrIRQ: .res 2

.segment "OAM"
SpriteZero:  .res 4
SpriteBlock: .res 4*4
GhostBlock:  .res 4*4

GameOverSprites: .res 8*4*4
GameOverOops: .res 8*4

.segment "BSS"
Palettes: .res 4*8
BufferedBlock: .res 4*4
rng_index: .res 1

Bin_Input: .res 3
Bin_Tiles: .res 6

FrameCount: .res 1

MMC5_OFFSET = $3C00

.macro SetIRQ Line, Addr
    lda #Line
    sta $5203
    lda #$80
    sta $5204

    lda #.lobyte(Addr)
    sta ptrIRQ+0
    lda #.hibyte(Addr)
    sta ptrIRQ+1
    cli
.endmacro

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR00"
    .incbin "tiles.chr"

.segment "CHR01"
    .incbin "tiles2.chr"

.segment "CHR02"
    .incbin "tiles3.chr"

.segment "CHR03"

.segment "PAGE00"

    .include "game.asm"

Screen_Playfield:
    ;.include "playfield.i"
    .include "playfield-rle.i"

DebugField:
    .include "debug-field.i"

PieceRng:
    .include "piece-rng.inc"

.segment "PRGINIT"

; Button Constants
BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

MMC5_MultA = $5205
MMC5_MultB = $5206

    .include "utils.asm"

IrqCall:
    jmp (ptrIRQ)

IRQ:
    pha
    txa
    pha
    tya
    pha

    lda AddressPointer1+0
    pha
    lda AddressPointer1+1
    pha

    lda AddressPointer2+0
    pha
    lda AddressPointer2+1
    pha

    bit $5204
    jsr IrqCall

    pla
    sta AddressPointer2+1
    pla
    sta AddressPointer2+0

    pla
    sta AddressPointer1+1
    pla
    sta AddressPointer1+0

    lda #$FF
    sta SleepingIrq

    pla
    tay
    pla
    tax
    pla
    rti

BareNmiHandler:
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
    rts

NMI:
    pha

    jsr NmiTrampoline

    bit $2002
    lda ScrollX
    sta $2005
    lda ScrollY
    sta $2005

    lda PpuControl
    sta $2000

    lda #$FF
    sta Sleeping

    inc FrameCount

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

    jsr MMC5_Init
    jmp InitMenu

MMC5_Init:
    ; PRG mode 0: one 32k bank
    lda #0
    sta $5100

    ; CHR mode 1: 4k pages
    lda #1
    sta $5101

    ; Vertical mirroring
    lda #$44
    sta $5105

    ; extended attr mode
    ;lda #%0000_0001
    lda #%0000_0000
    sta $5104
    rts


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

Palette_Sp:
    .byte $0F, $27, $00, $10

    .include "scores.asm"
    .include "menu.asm"
