
.include "nes2header.inc"
nes2mapper 5
nes2prg 4 * 16 * 1024
nes2chr 2 * 8 * 1024
nes2bram 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers
.feature addrsize

FAMISTUDIO_CFG_C_BINDINGS = 0

MMC5_OFFSET = $3C00 ; Offset from $2000

;DEBUG_PIECE = 0
;DEBUG_ROTATION = 0
;DEBUG_FIELD = 1
;DEBUG_BLOCK = 2
;DEBUG_FLASH = 0
;DEBUG_COLORS = 1

;
; Defaults
;

.ifdef DEBUG_COLORS
BG_COLOR = $20
.else
BG_COLOR = $0F
.endif

GHOST_FLASH = 0
SCREEN_SHAKE = 1

; Initial wait time in frames before repeating input
BUTTON_REPEAT_START = 15
; Held wait time in frames between repeated input
BUTTON_REPEAT = 3

TRUE = 1
FALSE = 0

.macro SetIRQ Line, Addr
    lda #.lobyte(Addr)
    sta ptrIRQ+0
    lda #.hibyte(Addr)
    sta ptrIRQ+1

    lda #Line
    sta $5203
    lda #$80
    sta $5204
    cli
.endmacro

.macro DisableIRQ
    lda #$00
    sta $5204
.endmacro

.macro EnableRam
    lda #$02
    sta $5102
    lda #$01
    sta $5103
.endmacro

.macro DisableRam
    lda #$00
    sta $5102
    sta $5103
.endmacro

.macro ResetNMI
    lda #.lobyte(BareNmiHandler)
    sta NmiHandler+0
    lda #.hibyte(BareNmiHandler)
    sta NmiHandler+1
.endmacro

.macro SetNMI Addr
    lda #.lobyte(Addr)
    sta NmiHandler+0
    lda #.hibyte(Addr)
    sta NmiHandler+1
.endmacro

.struct ScoreEntry
    Name  .byte 16
    Time  .byte 2
    Lines .byte 3
    Score .byte 3 ; maybe something else?
.endstruct

.enum InitIndex
Game
Scores
Modes
VsMode
Options
NewScore
Menu
PracticeMenu
ConfirmClear
.endenum

.enum GameBaseType
Standard
SingleBlock
TimeAttack
.endenum

.enum GameStandardArgs
Standard
Classic
NoHold
DirtyBoard
.endenum

.struct GameMode
BaseType .byte
TypeArg  .byte
HsIndex  .byte
.endstruct

.enum GameType
Standard
OnlyZ
OnlyS
OnlyT
OnlyL
OnlyJ
TimeLines
TimeScore
DirtyBoard
NoHold
Classic
.endenum

.enum MMSel
Practice
Classic
SingleBlock
TimeAttack
NoHold
DirtyBoard
Marathon
.endenum


.segment "ZEROPAGE"
Sleeping: .res 1
SleepingIrq: .res 1
AddressPointer1: .res 2
AddressPointer2: .res 2
AddressPointer3: .res 2
AddressPointer4: .res 2
NmiHandler: .res 2

TmpA: .res 1
TmpB: .res 1
TmpC: .res 1
TmpX: .res 1
TmpXX: .res 1
TmpY: .res 1
TmpYY: .res 1
TmpZ: .res 1
TmpZZ: .res 1

BinOutput: .res 4
IsPaused: .res 1

Controller: .res 2
Controller_Old: .res 2

ptrIRQ: .res 2

ScrollX:    .res 1
ScrollY:    .res 1

PpuControl: .res 1
PpuMask: .res 1

bcdInput:   .res 3  ; bin
bcdScratch: .res 4  ; bcd
bcdOutput:  .res 8  ; ascii

; Index into high scores tables
CurrentGameType: .res 1

CurrentScore: .tag ScoreEntry
TimeFrame: .res 1

LoadBoard_Addr: .res 2

.segment "OAM"
SpriteStart = *
;SpriteBlock: .res 4*4
;GhostBlock:  .res 4*4
SpriteP1: .res 4*4
SpriteGhostP1: .res 4*4
SpriteP2: .res 4*4
SpriteGhostP2: .res 4*4

GameOverSprites: .res 8*4*4
GameOverOops: .res 8*4
TSpinDebugSprite: .res 4

.segment "STACK"
Palettes: .res 4*8
LowBank: .res 1

PpuBuff_Vertical: .res 1
PpuBuff_Addr: .res 2
PpuBuff_Len: .res 1
PpuBuff_Data: .res 20

.segment "BSS"
;BufferedBlock: .res 4*4
rng_index: .res 1

Bin_Input: .res 3
Bin_Tiles: .res 6

FrameCount: .res 1

CurrentGameMode: .tag GameMode
LagNMI: .res 1
LagIRQ: .res 1

    .include "scores.asm"

    .include "utils.asm"

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
    .incbin "tiles4.chr"

.segment "PAGE_GAME"

    .include "game.asm"
    .include "vsmode.asm"

.ifdef DEBUG_FIELD
DebugField:
    .include "debug-field.i"
.endif

PieceRng:
    .include "piece-rng.inc"

.segment "PAGE_GAME2"

    .include "menu.asm"
    .include "mode-menu.asm"

    .include "basic-menu.asm"
    .include "options.asm"
    .include "menu-practice.asm"

Screen_Playfield:
    .include "playfield-rle.i"

Screen_PlayfieldVs:
    .include "playfield-vs.i"

Screen_Scores:
    .include "scores-screen.i"

Screen_NewHighScore:
    .include "new-high-score-a.i"

Screen_NewHighScore_Shifted:
    .include "new-high-score-b.i"

Screen_ModeMenu:
    .include "mode-menu.i"

Screen_Menu:
    .include "menu-screen.i"

.segment "PAGE_AUDIO"

    .include "famistudio_ca65.s"
    .include "audio/sfx.s"
    .include "audio/songs.s"

.segment "PAGE_01"

.segment "PAGE_INIT"

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

IrqCall:
    jmp (ptrIRQ)

IRQ:
    bit SleepingIrq
    bpl :+
    inc LagIRQ
    bit $5204
    rti
:

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

    lda TmpX
    pha
    lda TmpA
    pha

    bit $5204
    jsr IrqCall

    pla
    sta TmpA
    pla
    sta TmpX

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
    inc FrameCount

    bit Sleeping
    bpl :+
    inc LagNMI
    rti
:

    pha

    txa
    pha
    tya
    pha

    lda AddressPointer1+0
    pha
    lda AddressPointer1+1
    pha

    lda AddressPointer4+0
    pha
    lda AddressPointer4+1
    pha

    lda AddressPointer2+0
    pha
    lda AddressPointer2+1
    pha

    lda TmpA
    pha
    lda TmpX
    pha

    jsr NmiTrampoline

    lda PpuBuff_Addr+1
    beq @noBuff

    lda PpuBuff_Vertical
    beq :+
    lda #%0000_0100
    jmp :++
:
    lda #0
:
    ora PpuControl
    sta $2000

    lda PpuBuff_Addr+1
    sta $2006
    lda PpuBuff_Addr+0
    sta $2006

    ldy PpuBuff_Len
    ldx #0
:   lda PpuBuff_Data, x
    sta $2007
    inx
    dey
    bne :-

    lda #0
    sta PpuBuff_Addr+1
    sta PpuBuff_Addr+0
    sta PpuBuff_Vertical
@noBuff:

    lda PpuControl
    sta $2000

    lda PpuMask
    sta $2001

    bit $2002
    lda ScrollX
    sta $2005
    lda ScrollY
    sta $2005

    lda #$FF
    sta Sleeping

    jsr fs_Update

    pla
    sta TmpX
    pla
    sta TmpA

    pla
    sta AddressPointer2+1
    pla
    sta AddressPointer2+0

    pla
    sta AddressPointer4+1
    pla
    sta AddressPointer4+0

    pla
    sta AddressPointer1+1
    pla
    sta AddressPointer1+0

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

    jsr MMC5_Init
    jsr InitRam

    lda #$FF
    sta NewHsIndex

    lda #0
    sta ModeSelection
    sta SingleBlockId
    sta CurrentGameType
    sta EN_Cursor
    sta LagNMI
    sta LagIRQ

    sta AddressPointer4+0
    sta AddressPointer4+1

    sta LoadBoard_Addr+0
    sta LoadBoard_Addr+1

    lda #1 ; NTSC
    ldx #.lobyte(music_data_untitled)
    ldy #.hibyte(music_data_untitled)
    jsr fs_Init

    ldx #.lobyte(sounds)
    ldy #.hibyte(sounds)
    jsr fs_Sfx_Init

    lda #0
    jsr fs_Music_Play

    lda #InitIndex::Menu
    jmp GotoInit

MMC5_Init:
    ; PRG mode 2
    ; 8k  @ $6000-$7FFF
    ; 16k @ $8000-$BFFF
    ; 8k  @ $C000-$DFFF
    ; 8k  @ $E000-$FFFF
    lda #2
    sta $5100

    lda #%1000_0000 ; game
    sta $5115

    lda #%1000_0100 ; util
    sta $5116

    lda #$07 ; init
    sta $5117

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

MMC5_SelectLowBank:
    ; PRG mode 2
    ; 8k  @ $6000-$7FFF
    ; 16k @ $8000-$BFFF
    ; 8k  @ $C000-$DFFF
    ; 8k  @ $E000-$FFFF
    ;lda #2
    ;sta $5100
    sta LowBank
    asl a
    ora #$80
    sta $5115
    rts

MMC5_SelectHighBank:
    ;asl a
    ora #$80
    sta $5116
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

; Labels for all the clear types. Tiles and MMC5 metadata.
; The idea is these will be written line-by line above
; the playfield when clears happen.
ClearNames:
    ; Single
    .byte 5  ; length
    .word :+ ; data

    ; Double
    .byte 5
    .word :++

    ; Triple
    .byte 6
    .word :+++

    ; Quad
    .byte 9
    .word :++++

    ; Mini T-Spin
    .byte 3
    .word :+++++

    ; T-Spin
    .byte 5
    .word :++++++

    ; Back2Back
    .byte 8
    .word :+++++++

    ; Perfect Clear
    .byte 10
    .word :++++++++

    ; Single
:   .repeat 5, i ; tiles
        .byte $91+i
    .endrepeat

    .repeat 5 ; mmc5
        .byte $62
    .endrepeat

    ; Double
:   .repeat 5, i
        .byte $A1+i
    .endrepeat

    .repeat 5
        .byte $62
    .endrepeat

    ; Triple
:   .repeat 6, i
        .byte $B1+i
    .endrepeat

    .repeat 6
        .byte $62
    .endrepeat

    ; Quad
:   .repeat 9, i
        .byte $C1+i
    .endrepeat

    .repeat 9
        .byte $62
    .endrepeat

    ; Mini T-Spin (just "mini")
:   .repeat 3, i
        .byte $9D+i
    .endrepeat

    .repeat 3
        .byte $62
    .endrepeat

    ; T-Spin
:   .repeat 5, i
        .byte $98+i
    .endrepeat

    .repeat 5
        .byte $62
    .endrepeat

    ; Back2Back
:   .repeat 8, i
        .byte $A8 + i
    .endrepeat

    .repeat 4
        .byte $62
    .endrepeat
    .repeat 4
        .byte $82
    .endrepeat

    ; Perfect Clear
:   .repeat 6, i
        .byte $BA + i
    .endrepeat
    .repeat 4, i
        .byte $CA + i
    .endrepeat

    .repeat 10
        .byte $62
    .endrepeat

Palette_Bg:
    .byte $0F, $20, $00, $10

Palette_Sp:
    .byte $0F, $27, $00, $10

.enum FS
init
music_play
music_pause
music_stop
sfx_init
sfx_play
update
.endenum

.macro PushRegs
    pha
    txa
    pha
    tya
    pha
.endmacro

.macro PopRegs
    pla
    tay
    pla
    tax
    pla
.endmacro

fs_functions:
    .word famistudio_init
    .word famistudio_music_play
    .word famistudio_music_pause
    .word famistudio_music_stop
    .word famistudio_sfx_init
    .word famistudio_sfx_play
    .word famistudio_update

fs_Init:
    PushRegs
    lda #FS::init
    jmp fs_Trampoline

fs_Music_Play:
    PushRegs
    lda #FS::music_play
    jmp fs_Trampoline

fs_Music_Pause:
    PushRegs
    lda #FS::music_pause
    jmp fs_Trampoline

fs_Music_Stop:
    PushRegs
    lda #FS::music_stop
    jmp fs_Trampoline

fs_Sfx_Init:
    PushRegs
    lda #FS::sfx_init
    jmp fs_Trampoline

fs_Sfx_Play:
    PushRegs
    lda #FS::sfx_play
    jmp fs_Trampoline

fs_Update:
    PushRegs
    lda #FS::update
    jmp fs_Trampoline

fs_Trampoline:
    asl a
    tax
    lda fs_functions+0, x
    sta AddressPointer4+0
    lda fs_functions+1, x
    sta AddressPointer4+1

    lda LowBank
    pha

    lda #1
    jsr MMC5_SelectLowBank

    pla
    sta LowBank

    lda #5
    jsr MMC5_SelectHighBank

    PopRegs
    jsr @jmp

    lda #4
    jsr MMC5_SelectHighBank

    lda LowBank
    jsr MMC5_SelectLowBank
    rts

@jmp:
    jmp (AddressPointer4)
