;****************************************************************
;
; Compression Example - Memotech MTX ver 1.00 (C) Electric Adventures 2026
;
;****************************************************************
;cpu z80

; Include MTX defined values
    INCLUDE "MTX-Include.ASM"

; Set header for a MTX Run file
;ORG $40fc
;    dw ROM_START
;    dw ROM_END-ROM_START

; set header for a MTX COM file
    ORG $0100

ROM_START:
	jp START
    db 0,0,0,0,0
    dw NMI_Handler
    dw NULL_VECTOR
    dw NULL_VECTOR
    DW ROM_START

NULL_VECTOR:
    ei
    reti

Z80_CTC equ $08

;
; Start of application logic
START:
    di
    im 2
    ld a,ROM_START>>8
	ld i,a
	ld a,$03	; Reset Z80 CTC
	out (Z80_CTC+0),a
	out (Z80_CTC+1),a
	out (Z80_CTC+2),a
	out (Z80_CTC+3),a
	out (Z80_CTC+0),a
	out (Z80_CTC+1),a
	out (Z80_CTC+2),a
	out (Z80_CTC+3),a
	ld a,$08	; Interrupt vector offset
	out (Z80_CTC+0),a	

    ld a,$25	; Disable channel 2 interrupt.
	out (Z80_CTC+2),a
	ld a,$9c
	out (Z80_CTC+2),a
	ld a,$25	; Disable channel 1 interrupt.
	out (Z80_CTC+1),a
	ld a,$9c
	out (Z80_CTC+1),a
	ld a,$c5	; Enable channel 0 interrupt (VDP).
	out (Z80_CTC+0),a
	ld a,$01
	out (Z80_CTC+0),a

    ; Init ram
	ld hl,RAMSTART
	ld de,RAMSTART+1
	ld bc,RAMEND-RAMSTART-1
	ld (hl),0
	ldir

    ; Initialise sound
    CALL DISABLE_PSG

    ; Initialise sound
	LD	B,SoundDataCount	;Max number of active voices+effects
	LD	HL,SoundAddrs
	CALL    INIT_SOUND

    ; initialise clock
    LD	HL,TIMER_TABLE
    LD	DE,TIMER_DATA_BLOCK
    CALL INIT_TIMER

    ; initialise joysticks and keyboard data
    xor a
	ld (joy1_data),a

    LD A,1
    LD (SKIPMUSIC),A

    ; Set screen mode 2,2 (16x16 sprites)
    CALL SETSCREEN2

    ; Seed random numbers with a fixed number (nothing else to use?)
    LD HL,1967
    CALL SEED_RANDOM

    ;Enable timers
    CALL CREATE_TIMERS

    ; Do all our VRAM setup
    ; NMI is currently disabled
    CALL DISABLE_NMI
    ; Send the two sprite definitions to the VDP
    LD HL,VRAM_SPRGEN
    LD DE,SPDATA
    LD BC,32*2
    CALL LDIRVM

    ; Clear the screen
    CALL CLEARPAT

    ; load in our compressed patterns and colours
    LD HL,TILESET_1_PAT_D1
    LD DE,VRAM_PATTERN
    CALL dan1rvram
    LD HL,TILESET_1_PAT_D1
    LD DE,VRAM_PATTERN + 8*256
    CALL dan1rvram
    LD HL,TILESET_1_PAT_D1
    LD DE,VRAM_PATTERN + 16*256
    CALL dan1rvram
    LD HL,TILESET_1_COL_D1
    LD DE,VRAM_COLOR
    CALL dan1rvram
    LD HL,TILESET_1_COL_D1
    LD DE,VRAM_COLOR + 8*256
    CALL dan1rvram
    LD HL,TILESET_1_COL_D1
    LD DE,VRAM_COLOR + 16*256
    CALL dan1rvram



MAIN_SCREEN:
    ; Read joysticks to clear any false reads
    ;CALL JOYTST

    ; Initial Seed random numbers with a random number from BIOS
    ;CALL RAND_GEN
    ;CALL SEED_RANDOM

	; disable interrupts
    CALL DISABLE_NMI

    ; Clean up in case the game left anything on screen
    CALL CLEARSPRITES
    CALL SPRWRT
    
    ; Clear the screen
    CALL CLEARPAT
    ; load compressed screen layout
    LD HL,SL__D1
    LD DE,VRAM_NAME
    CALL dan1rvram  

    XOR A
    LD (SKIPMUSIC),A

    LD HL,VDU_WRITES
    CALL SET_VDU_HOOK
    CALL ENABLE_NMI

    ; Set initial position, colour and shape of the ball
    LD HL,04040h
    LD (SPRTBL),HL
    LD HL,00500h
    LD (SPRTBL+2),HL

    ; Set initial position, colour and shape of the bat
    LD HL,080A0H
    LD (SPRTBL+4),HL
    LD HL,00604h
    LD (SPRTBL+6),HL

    ; set initial velocity of ball (dx = 1, dy = 1)
    LD HL,00101h
    LD (BALL),HL

    ; Main game logic loop
MLOOP:
    ; check that a base tick has occurred
    ; ensures consistent movement speed between 50 & 60Hz systems
    LD	A,(TickTimer)
    CALL	TEST_SIGNAL
    OR	A
    JR Z,MLOOP

    CALL MOVE_BALL
    CALL MOVE_PLAYER
    JR MLOOP

; Move the player
MOVE_PLAYER:
    LD A,(joy1_data)
    BIT 3,A
    JR NZ,NRIGHT
    ; move to the right
    LD A,(SPRTBL+5)
    CP 239
    RET Z
    INC A
    LD (SPRTBL+5),A
    RET
NRIGHT:
    BIT 2,A
    RET NZ
    ; move to the left
    LD A,(SPRTBL+5)
    CP 0
    RET Z
    DEC A
    LD (SPRTBL+5),A
    RET

; move the Ball
MOVE_BALL:
    ; change the current y position
    LD A,(SPRTBL)
    LD B,A
    LD A,(BALL)
    ADD A,B
    LD (SPRTBL),A
    CP 0
    JR NZ, NOTTOP
    ; hit the top
    LD A,1
    LD (BALL),A
    LD B,1
    CALL PLAY_IT
    JR YDONE
NOTTOP:
    CP 175
    JR NZ, YDONE
    LD A,255
    LD (BALL),A
    LD B,1
    CALL PLAY_IT
YDONE:
    ; change the current x position
    LD A,(SPRTBL+1)
    LD B,A
    LD A,(BALL+1)
    ADD A,B
    LD (SPRTBL+1),A
    CP 0
    JR NZ, NOTLEFT
    ; hit the left
    LD A,1
    LD (BALL+1),A
    LD B,1
    CALL PLAY_IT
    JR XDONE
NOTLEFT:
    CP 239
    JR NZ, XDONE
    LD A,255
    LD (BALL+1),A
    LD B,1
    CALL PLAY_IT
XDONE:
    RET

; This is our routine called every VDP interrup
; - Do all VDP writes here to avoid corruption
; Note:
; - The included VDP routine is already calling the 
;   sound update routines, and writing the sprite data
;   table to VRAM.
VDU_WRITES:
    RET

    include "Compression-Patterns.asm"


SPDATA:
    db 003h,00Fh,01Fh,03Fh,07Fh,07Fh,0FFh,0FFh
    db 0FFh,0FFh,07Fh,07Fh,03Fh,01Fh,00Fh,003h
    db 0C0h,0F0h,0F8h,0FCh,0FEh,0FEh,0FFh,0FFh
    db 0FFh,0FFh,0FEh,0FEh,0FCh,0F8h,0F0h,0C0h
    db 000,000,000,000,000,000,000,000
    db 000,000,000,000,000,000,255,255
    db 000,000,000,000,000,000,000,000
    db 000,000,000,000,000,000,255,255

;**************************************************************************************************
; Sound and music data area
;**************************************************************************************************

; Bounce
bounce:
    DB 081h, 054h, 010h, 002h, 023h, 007h
    DB $90  ; end
    DW 0000h


;**************************************************************************************************
; Sound settings
;**************************************************************************************************
SoundDataCount	  EQU	7
Len_SoundDataArea EQU	10*SoundDataCount+1	;7 data areas
SoundAddrs:
	DW	bounce,SoundDataArea     ; 1  ball bounce sound
	DW  0,0

;**************************************************************************************************
; Standard Libraries
;**************************************************************************************************

    include "MTX-Lib.ASM"

;ROM_END:	EQU $

;**************************************************************************************************
; RAM Definitions
;**************************************************************************************************

    ORG RAMSTART

BALL:       DS 2

; Sound Data area - 7 songs
SoundDataArea: DS Len_SoundDataArea

RAMEND EQU $
