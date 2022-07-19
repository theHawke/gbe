; gb_bios.bin disassembly (hand-disassebled)

.org 0
    ; set up stack pointer
    LD SP $FFFE

    ; clear VRAM to 0
    XOR A
    LD HL $9FFF
.label VRAM_CLEAR_LOOP
    LD (HL-) A
    BIT 7 H
    JR NZ @VRAM_CLEAR_LOOP

    ; set up several control registers
    LD HL $FF26
    LD C $11
    LD A $80
    LD (HL-) A ; NR52 -> sound ON
    LDH (C) A ; NR11 -> Sound 1 50% DC
    INC C
    LD A $F3
    LDH (C) A ; NR12 -> Sound 1 max volume, attenuate 3
    LD (HL-) A ; NR51 -> Sound output terminal (Sound 1 to both output channels)
    LD A $77
    LD (HL) A ; NR50 -> Sound channel control -> both channels max volume, no passthrough
    LD A $FC
    LDH ($47) A ; BGP: 00 -> 00, other -> 11 

    ; decompress compressed logo tiles and copy to VRAM
    ; logo is stored in 2x2 pixel blocks and 1-bit color -> 2 bytes per tile
    LD DE $0104
    LD HL $8010
.label LOGO_DECOMPRESS_LOOP
    LD A (DE)
    CALL @DECOMPRESS_0
    CALL @DECOMPRESS_1
    INC DE
    LD A E
    CP $34
    JR NZ @LOGO_DECOMPRESS_LOOP

    ; last logo tile is not stored in game rom, 1-bit color, but full resolution
    LD DE $00D8
    LD B $08
.label LAST_LOGO_TILE_LOOP
    LD A (DE)
    INC DE
    LD (HL+) A
    INC (HL)
    DEC B
    JR NZ @LAST_LOGO_TILE_LOOP

    ; write background tilemap
    LD A $19
    LD ($9910) A
    LD HL $992F
.label BG_LOOP
    LD C $0C
.label BG_INNER_LOOP
    DEC A
    JR Z @BG_LOOP_END
    LD (HL-) A
    DEC C
    JR NZ @BG_INNER_LOOP
    LD L $0F
    JR @BG_LOOP
.label BG_LOOP_END

    ; start up graphics and sound
    LD H A
    LD A $64
    LD D A ; LOGO_SCROLL_OUTER_LOOP counter
    LDH ($42) A ; SCY: $64 --> 0
    LD A $91
    LDH ($40) A ; LCDC

    INC B ; B = 1 -> logo moves down

.label LOGO_SCROLL_OUTER_LOOP
    LD E $02
.label FRAME_WAIT_LOOP_0
    ; waste ~3 Frames 
    LD C $0C
.label FRAME_WAIT_LOOP_1
    ; wait for line 144, i.e. VBLANK
    LDH A ($44) ; LY
    CP $90
    JR NZ @FRAME_WAIT_LOOP_1
    DEC C
    JR NZ @FRAME_WAIT_LOOP_1
    DEC E
    JR NZ @FRAME_WAIT_LOOP_0

    LD C $13
    INC H
    LD A H
    LD E $83
    CP $62
    JR Z @CHANGE_SOUND
    LD E $C1
    CP $64
    JR NZ @SKIP_SOUND
.label CHANGE_SOUND
    ; sound control
    ; set lower 8 bits of frequency
    LD A E
    LD (C) A
    INC C
    ; set higher 8 bits of frequency and (re-)start sound
    LD A $87
    LD (C) A
.label SKIP_SOUND
    ; move logo downwards (decrease SCY -> move view up on BG)
    LDH A ($42)
    SUB B
    LDH ($42) A

    DEC D
    JR NZ @LOGO_SCROLL_OUTER_LOOP

    DEC B
    JR NZ @PERFORM_CHECKS_AND_START

    LD D $20
    JR @LOGO_SCROLL_OUTER_LOOP

.label DECOMPRESS_0
    LD C A
.label DECOMPRESS_1
    LD B $04
.label DECOMPRESS_LOOP
    PUSH BC
    RL C
    RL A
    POP BC
    RL C
    RL A
    DEC B
    JR NZ @DECOMPRESS_LOOP
    LD (HL+) A
    INC (HL)
    LD (HL+) A
    INC (HL)
    RET


.org $A8
; logo data...

.org $D8
; logo data last tile

.org $E0
.label PERFORM_CHECKS_AND_START
    LD HL $0104
    LD DE 00A8
.label LOGO_CHECK_LOOP
    LD A (DE)
    INC DE
    CP (HL)
.label LOGO_FAIL
    JR NZ @LOGO_FAIL
    INC HL
    LD A L
    CP $34
    JR NZ @LOGO_CHECK_LOOP

    LD B $19
    LD A B
.lable CHECKSUM_LOOP
    ADD A (HL)
    INC HL
    DEC B
    JR NZ @CHECKSUM_LOOP
    ADD A (HL)
.label CHECKSUM_FAIL
    JR NZ @CHECKSUM_FAIL

    LD A $01
    LDH ($50) A ; LOCK BOOTROM