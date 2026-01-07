;****************************************************************
; Memotech MTX SUBROUTINES ver 1.00 (C) Electric Adventures 2026
;****************************************************************

; HL = Hook Address
SET_VDU_HOOK:
   LD A,0cdh
   LD (VDU_HOOK),A
   LD (VDU_HOOK+1),HL
   LD A,0c9h
   LD (VDU_HOOK+3),A
   RET

DISABLE_NMI:
    di
    ld bc,$a201
    call WRTVDP
    ;ei
    ret

ENABLE_NMI:
    di
    ld bc,$e201
    call WRTVDP
    ei
    ret

; Set the name table to default values
; DE = VRAM Offset
SET_DEF_NAME_TBL:
    ld      c,CTRL_PORT
    out     (c),e
    set     6,d
    out     (c),d
    ld      c,DATA_PORT
    ld      d,3
SDNT1:
    xor     a
SDNT2:
    out     (c),a
    nop
    inc     a
    jp      nz,SDNT2
    dec     d
    jp      nz,SDNT1
    ret

; Poll the keyboard to capture player one and two key presses
POLL_KEYBOARD:
	ld bc,$ffff

    ld a,$fb
    out (KBD_CTRL),a	; Select keyboard row.
    ex (sp),hl
    ex (sp),hl
    in a,(KBD_CTRL)	; Read keyboard data.
    bit 7,a
    jr nz,$+4
    res 0,b         ; Up direction.
    bit 3,a		; Y
    jr nz,$+4
    res 0,c		; Up direction.

    ld a,$ef
    out (KBD_CTRL),a	; Select keyboard row.
    ex (sp),hl
    ex (sp),hl
    in a,(KBD_CTRL)	; Read keyboard data.
    bit 7,a
    jr nz,$+4
    res 1,b         ; Right key.
    ld a,$bf
    out (KBD_CTRL),a	; Select keyboard row.
    ex (sp),hl
    ex (sp),hl
    in a,(KBD_CTRL)	; Read keyboard data.
    bit 7,a
    jr nz,$+4
    res 2,b         ; Down key.
    ld a,$f7
    out (KBD_CTRL),a	; Select keyboard row.
    ex (sp),hl
    ex (sp),hl
    in a,(KBD_CTRL)	; Read keyboard data.
    bit 7,a
    jr nz,$+4
    res 3,b         ; Left key.
    ld a,$df
    out (KBD_CTRL),a	; Select keyboard row.
    ex (sp),hl
    ex (sp),hl
    in a,(KBD_CTRL)	; Read keyboard data.
    bit 7,a         ; Home key.
    jr nz,$+4
    res 6,b         ; Button 1

    ld a,$7F
    out (KBD_CTRL),a	; Select keyboard row.
    ex (sp),hl
    ex (sp),hl
    in a,(KBD_CTRL)	; Read keyboard data.
    bit 2,a		; B
    jr nz,$+4
    res 0,c         ; Up key.
    bit 1,a		; C
    jr nz,$+4
    res 1,c         ; Right key.
    bit 3,a		; M
    jr nz,$+4
    res 2,c         ; Down key.
    bit 0,a		; Z
    jr nz,$+4
    res 3,c         ; Left key.
	bit 5,a		; -
	jr nz,$+4
	res 7,c		; Button 2 for player 2.
	bit 7,a		; -
	jr nz,$+4
	res 7,b		; Button 2 for player 1.
        in a,(KBD_DATA)	; Read keyboard data.
        bit 0,a         ; Space
        jr nz,$+4
        res 6,c         ; Button 1.
	ld a,b
	cpl
	ld (joy1_data),a
	ld a,c
	cpl
	ld (joy2_data),a

	ld a,$fe
	out (KBD_CTRL),a
	ex (sp),hl
	ex (sp),hl
	in a,(KBD_CTRL)
	rra
	ld b,1
	jr nc,.mt1
	rra
	ld b,3
	jr nc,.mt1
	rra
	ld b,5
	jr nc,.mt1
	rra
	ld b,7
	jr nc,.mt1
	rra
	ld b,9
	jr nc,.mt1
	ld a,$df
	out (KBD_CTRL),a
	ex (sp),hl
	ex (sp),hl
	in a,(KBD_CTRL)
	bit 6,a
	ld b,11
	jr z,.mt1
	ld a,$fd
	out (KBD_CTRL),a
	ex (sp),hl
	ex (sp),hl
	in a,(KBD_CTRL)
	rra
	rra
	ld b,2
	jr nc,.mt1
	rra
	ld b,4
	jr nc,.mt1
	rra
	ld b,6
	jr nc,.mt1
	rra
	ld b,8
	jr nc,.mt1
	rra
	ld b,0
	jr nc,.mt1
	in a,($06)
	rra
	ld b,10
	jr nc,.mt1
	ld b,15
.mt1:
	ld a,b
	ld (key1_data),a

    ret


;===========================================================================
; Standard RLE to VRAM
; HL = Source data
;      1st byte data, 2nd byte runlength, if runlength=0 end of data
; DE = VRam starting location
;===========================================================================
RLE_TO_VRAM:
    ld      c,CTRL_PORT
    out     (c),e
    set     6,d
    out     (c),d
    ld      c,DATA_PORT
RTV1:    
    ld      a,(hl)
    inc     hl
    ld d,a ; save our data byte
    ld      a,(hl)
    inc hl
    cp 0 ;  zero runlength - end of data
    ret z
    cp 1
    jp nz, RTV2
    ; single byte - output
    out (c),d
    jp RTV1
RTV2:
    ld b,a
RTV3:
    out (c),d
    nop
    nop
    djnz RTV3
    jp RTV1

;===========================================================================
; DAN1 + RLE to VRAM
; 2016 Daniel Bienvenu, Canada
; HL = Source
; DE = Destination (in VRAM)
;===========================================================================
dan1rvram:
    ; Set Write in VRAM at DE
    ld      c, $BF
    out     (c), e
    set     6, d
    out     (c), d
    res     6, d
    ; Init. Read bits
    ld      a, $80

; Copy literal byte
dan1r_copy_byte:
    ld      b, $01
    jr      dan1r_literals2main

dan1r_special:
    pop     de
    call        getbit
    ret     nc        ; EXIT
    ld      b, (hl) ; Load counter value as a byte
    inc     hl
    inc     b
    call        dan1r_literals
    ld      b, $1A
dan1r_literals2main:
    call        dan1r_literals

; Main loop
dan1r_main_loop:
    call        getbit                    ; Check next bit
    jr      c,dan1r_copy_byte
    
; Elias gamma decoding + Special marker
    push    de
    ld      de, $0001
    ld      b,d 
dan1r_eliasgamma_0:
    inc     b
    call        getbit          ; Check next bit
    jr      c, dan1r_eliasgamma_value
    bit     4,b
    jr      nz, dan1r_special   ; Special marker "0000000000000000"
    jr      dan1r_eliasgamma_0
dan1r_eliasgamma_value_loop:
    call        getbite         ; Load next bit into DE
    rl      d
dan1r_eliasgamma_value:
    djnz        dan1r_eliasgamma_value_loop
    push    de
    pop     bc          ; BC = LENGTH
    
; Get Offset value
    ld      d, $00      ; Reset Offset to $0000
    
; ON LEN GOTO TWO_OFFSETS, THREE_OFFSETS
; GOTO FOUR_OFFSETS

    ex      af,af'
    ld      a,b
    or      a
    jr      z, dan1r_bzero
    ld      a, $03
dan1r_bzero:
    or      c
    ld      e, a
    ex      af, af'
    dec     e
    jr      z, dan1r_offset2
    dec     e
    jr      z, dan1r_offset3
    ld      e, d

dan1r_offset4:
    call        getbit          ; Check next bit
    jr      nc, dan1r_offset3
    call        getnibblee          ; Get next nibble -> E
    inc     e
    ld      d,e             ; D = E + 1
    jr      dan1r_offset3a
dan1r_offset3:
    call        getbit          ; Check next bit
    jr      nc, dan1r_offset2
dan1r_offset3a:
    ld      e, (hl)         ; Load offset as a byte (8 bits) -> E
    inc     hl
    ex      af, af'
    ld      a,e
    add     a, $12
    ld      e,a
    jr      nc, dan1r_offset3b
    inc     d
dan1r_offset3b:
    ex      af,af'
    jr      dan1r_copy_from_offset
dan1r_offset2:
    call        getbit          ; Check next bit
    jr          nc, dan1r_offset1
    call        getnibblee          ; Get next nibble -> E
    inc     e
    inc     e
    jr      dan1r_copy_from_offset
dan1r_offset1:
    call        getbit          ; Load next bit -> E
    rl      e
    
; Copy previously seen bytes
dan1r_copy_from_offset:
    ex      (sp), hl                ; Store source, Restore destination
    push    hl                              ; Store destination
    scf
    sbc     hl, de                  ; HL = source = destination - offset - 1
    pop     de                      ; DE = destination
                                            ; BC = count
    ; COPY BYTES
    ex      af,af'
    set     6,d
dan1r_copybytes_loop:
    push    bc
    ld      c, $BF
    out     (c), l
    nop
    out     (c), h
    inc     hl
    nop
    nop
    in      a, ($BE)
    nop
    nop
    nop
    out     (c), e
    nop
    out     (c), d
    inc     de
    nop
    nop
    out     ($BE), a
    pop     bc
    dec     bc
    ld      a,b
    or      c
    jr      nz, dan1r_copybytes_loop
    res     6,d
    ex      af,af'
    pop     hl      ; Restore source address
    jp      dan1r_main_loop
    
dan1r_literals:
    ld      c, $BE
dan1r_literals_loop:
    outi
    inc     de
    jr      nz, dan1r_literals_loop
    ret

getnibblee:
    call    getbite ; Load next bit -> E
    call    getbite ; Load next bit -> E
    call    getbite ; Load next bit -> E
getbite:
    call    getbit  ; Load next bit -> E
    rl      e   
    ret

getbit:
    add     a, a
    ret     nz
    ld      a, (hl)
    inc     hl
    rla
    ret

;===========================================================================
; Pletter to VRAM
; HL = Source data
; DE = VRam starting location
;===========================================================================
PLETTER_TO_VRAM:
    ; now decompress
    ld a,(hl)
    inc hl
    exx
    ld de,0
    add a,a
    inc a
    rl e
    add a,a
    rl e
    add a,a
    rl e
    rl e
    ld hl,modes
    add hl,de
    ld e,(hl)
    ld ixl,e
    inc hl
    ld e,(hl)
    ld ixh,e
    ld e,1
    exx
    ld iy,loop
literal:
    ldi
loop:
    add a,a
    call z,getbit1
    jr nc,literal
    exx
    ld h,d
    ld l,e
getlen:
    add a,a
    call z,getbitexx
    jr nc,lenok
lus:
    add a,a
    call z,getbitexx
    adc hl,hl
    ret c
    add a,a
    call z,getbitexx
    jr nc,lenok
    add a,a
    call z,getbitexx
    adc hl,hl
    ret c
    add a,a
    call z,getbitexx
    jp c,lus
lenok:
    inc hl
    exx
    ld c,(hl)
    inc hl
    ld b,0
    bit 7,c
    jp z,offsok
    jp (ix)

mode6:
    add a,a
    call z,getbit1
    rl b
mode5:
    add a,a
    call z,getbit1
    rl b
mode4:
    add a,a
    call z,getbit1
    rl b
mode3:
    add a,a
    call z,getbit1
    rl b
mode2:
    add a,a
    call z,getbit1
    rl b
    add a,a
    call z,getbit
    jr nc,offsok
    or a
    inc b
    res 7,c
offsok:
    inc bc
    push hl
    exx
    push hl
    exx
    ld l,e
    ld h,d
    sbc hl,bc
    ; now output to vram
    ld      c,CTRL_PORT
    out     (c),l
    set     6,h
    out     (c),h
    pop bc
    pop hl
    push hl
wrtloop:
    ld a,(hl)
    out (DATA_PORT),a
    inc hl
    dec bc
    ld a,b
    or c
    jp nz,wrtloop
    ;ldir
    pop hl
    jp (iy)

getbit1:
    ld a,(hl)
    inc hl
    rla
    ret

getbitexx:
    exx
    ld a,(hl)
    inc hl
    exx
    rla
    ret

modes:
  dw offsok
  dw mode2
  dw mode3
  dw mode4
  dw mode5
  dw mode6


;===========================================================================
; RLE to VRAM
; HL = Source data
; DE = VRam starting location
;===========================================================================
RLE2VRAM:
    ld      c,CTRL_PORT
    out     (c),e
    set     6,d
    out     (c),d
    ld      c,DATA_PORT
RLE2V0:
    ld      a,(hl)
    inc     hl
    cp      0ffh
    ret     z
    bit     7,a
    jr      z,RLE2V2
    and     07fh
    inc     a
    ld      b,a
    ld      a,(hl)
    inc     hl
RLE2V1:
    out     (c),a
    nop
    nop
    djnz    RLE2V1
    jr      RLE2V0
RLE2V2:
    inc     a
    ld      b,a
RLE2V3:
    outi
    jr      z,RLE2V0
    jp      RLE2V3
;
; Uncompress RLE data into RAM
; HL = Source data
; DE = Destination
RLE2RAM:
RLE2R0:
    ld      a,(hl)
    inc     hl
    cp      0ffh
    ret     z
    bit     7,a
    jr      z,RLE2R2
    and     07fh
    inc     a
    ld      b,a
    ld      a,(hl)
    inc     hl
RLE2R1:
    ld      (de),a
    inc     de
    djnz    RLE2R1
    jr      RLE2R0
RLE2R2:
    inc     a
    ld      b,a
    ldir
    jr      RLE2R0

;===========================================================================
; unRLEWBtoVRAM v1.1 (26 jun 2014)
; Function : Unpack WB RLE to VRAM
;
; Input    : HL - source RAM RLE data address
;            DE - target VRAM address
;
; $80 nn dd            ; run of n consecutive identical bytes ($1>$FE), value dd
; $80 $0               ; for one $80 value
; $80 $FF              ; end of data block
; <any other value>    ; raw data
;===========================================================================

RLECONTROL: EQU   080h

unRLEWBtoVRAM:
  ; set VRAM addr
  ex de,hl
  call SETWRT
  ex de,hl

ANALYZE:
  ld    A,[HL]         ; get byte
  cp    RLECONTROL
  jr    NZ,WriteByte   ; if raw

  inc   HL             ; get next byte
  ld    A,[HL]
  or    A
  jr    Z,WriteCONTROL ;if A=0 then write one $80  ($80 $0)
  cp    0FFh            ;if A=$FF ($80 $FF)
  ret   Z              ;then exit

  ;$80 nn dd
  inc   A              ;2 to 255
  ld    B,A
  inc   HL
  ld    A,[HL]         ;get value

doRLE:
  out   [DATA_PORT],A    ;write in VRAM
  nop
  nop
  djnz  doRLE

  inc   HL
  jr    ANALYZE

WriteCONTROL:
  ld    A,RLECONTROL  ;write CONTROL value

WriteByte:
  out   [DATA_PORT],A   ;write in VRAM
  ;nop
  inc   HL
  jr    ANALYZE

; Write to VDP, port in B, value in C
WRTVDP:
    LD A,B
    OUT (CTRL_PORT),A
    LD A,C
    OR 80h
    OUT (CTRL_PORT),A
    RET

;
; Set write to Video Ram
; HL = VRAM Address
SETWRT:
    LD A,L
    OUT (CTRL_PORT),A
    LD A,H
    AND $3f
    OR $40
    OUT (CTRL_PORT),A
    RET

;
; Set read to Video Ram
; HL = VRAM Address
SETRD:
    LD A,L
    OUT (CTRL_PORT),A
    LD A,H
    AND 3Fh
    OUT (CTRL_PORT),A
    RET

;
; Read ^[STOP], Z FLAG SET IF TRUE
BREAKX:
    RET ; disable
    LD A,22
    OUT (0AAh),A
    IN A,(0A9h)
    BIT 1,A
    RET NZ
    LD A,23
    OUT (0AAh),A
    IN A,(0A9h)
    BIT 4,A
    RET

;
; Load a block of memory to VRAM
; HL = VRAM Address
; DE = RAM Address
; BC = Length
LDIRVM:
    CALL SETWRT
.LOOP:
    LD A,(DE)
    OUT (DATA_PORT),A
    NOP
    INC DE
    DEC BC
    LD A,C
    OR B
    JP NZ,.LOOP
    RET

;
; Fill a section of VRAM with value in A
; HL = VRAM Address
; BC = Length
FILVRM:
    LD E,A
    CALL SETWRT
.LOOP:
    LD A,E
    OUT (DATA_PORT),A
    DEC BC
    LD A,B
    OR C
    JP NZ,.LOOP
    RET
;
; Write Sprite positions to VRAM
SPRWRT:
    LD A,(SPRORDER)
    BIT 0,A
    JR NZ,SW1
    ; write sprites normal order
    SET 0,A
    LD (SPRORDER),A
    LD HL,VRAM_SPRATTR
    LD DE,SPRTBL
    LD BC,80h
    CALL LDIRVM
    RET
SW1:
    ; write sprites reverse order
    RES 0,A
    LD (SPRORDER),A
    LD HL,VRAM_SPRATTR
    CALL SETWRT
    LD HL,SPRTBL+80h-4
    LD B,32
    LD DE,7
SW2:
    LD A,(HL)
    INC HL
    OUT (DATA_PORT),A
    LD A,(HL)
    INC HL
    OUT (DATA_PORT),A
    LD A,(HL)
    INC HL
    OUT (DATA_PORT),A
    LD A,(HL)
    OUT (DATA_PORT),A
    XOR A
    SBC HL,DE
    DJNZ SW2
    RET

READ_REGISTER:
    IN A,(CTRL_PORT)
    RET

WRITE_REGISTER:
    LD A,C
    OUT (CTRL_PORT),A
    LD A,B
    AND 07h
    OR 080h
    OUT (CTRL_PORT),A
    RET

;
; Setup Screen 2,2
SETSCREEN2:
    LD BC,0002h	;Reg 0: Mode 2
    CALL WRITE_REGISTER
    LD BC,0206h        ; Name table 1800h
    CALL WRITE_REGISTER
    LD BC,03ffh        ; Colour table 2000h
    CALL WRITE_REGISTER
    LD BC,0403h        ; Pattern table 0000h
    CALL WRITE_REGISTER
    LD BC,0536h        ; Sprite attribute table 1b00h
    CALL WRITE_REGISTER
    LD BC,0607h        ; Sprite pattern table 3800h
    CALL WRITE_REGISTER
    LD BC,0701h        ; Base colours
    CALL WRITE_REGISTER
    LD BC,01a2h	;Reg 1: Mode 2, 16k, no interrupts, 16x16 sprites
    CALL WRITE_REGISTER
    RET

DISABLE_PSG:
	ld a, $9F
	out (PSG), a
	ld a, $BF	
	out (PSG), a
	ld a, $DF
	out (PSG), a
	ld a, $FF
	out (PSG), a	
    RET


;
; Play a sound, protects the calling routine from common
; registers being changed.
; B = Sound to play
SOUND:
    PUSH IX
    PUSH IY
    PUSH HL
    PUSH DE
    ;CALL PLAY_IT
    POP DE
    POP HL
    POP IY
    POP IX
    RET

;
; Output a character to the screen nametable
; (HL) contains the character to output
;
PRINTIT:
    XOR A ; clear A
    RLD   ; rotate left out of (HL) into A
    INC A
    OUT (DATA_PORT),A
    DEC A
    RLD   ; rotate left out of (HL) into A
    INC A
    OUT (DATA_PORT),A
    DEC A
    RLD
    RET

; Output a big character to the screen nametable
; (HL) contains the character to output
;
PRINTIT_BIGTOP:
    XOR A ; clear A
    RLD   ; rotate left out of (HL) into A
    ADD A,126
    OUT (DATA_PORT),A
    SUB 126
    RLD   ; rotate left out of (HL) into A
    ADD A,126
    OUT (DATA_PORT),A
    SUB 126
    RLD
    RET
;
; Output a big character to the screen nametable
; (HL) contains the character to output
;
PRINTIT_BIGBOTTOM:
    XOR A ; clear A
    RLD   ; rotate left out of (HL) into A
    ADD A,136
    OUT (DATA_PORT),A
    SUB 136
    RLD   ; rotate left out of (HL) into A
    ADD A,136
    OUT (DATA_PORT),A
    SUB 136
    RLD
    RET

;
; Clear the sprites from the screen (set Y=209)
CLEARSPRITES:
	LD B,80h
	LD DE,SPRTBL
CSPR1:
    LD A,209
	LD (DE),A
	INC DE
	DJNZ CSPR1
	RET
;
; Clear the VDP Pattern table (clears screen)
CLEARPAT:
	LD HL,VRAM_NAME
	LD BC,768
	XOR A
	CALL FILVRM
    RET
;
; Create and enable standard timers
CREATE_TIMERS:
	LD	HL,(AMERICA)	;How long a second is
	SRA L
	LD	H,0
	LD	A,1	;set to repeating
	CALL	REQUEST_SIGNAL
	LD	(HalfSecTimer),A		;Happens once per half second
	LD	HL,(AMERICA)	;How long a second is
	SRA L
	SRA L
	LD	H,0
	LD	A,1	;set to repeating
	CALL	REQUEST_SIGNAL
	LD	(QtrSecTimer),A		;Happens once per quarter second
    LD  HL,(AMERICA)    ;How long a second is
    SRA L
    SRA L
    SRA L
    ;SRA L
    LD  H,0
    LD  A,1 ;set to repeating
    CALL    REQUEST_SIGNAL
    LD  (EigthSecTimer),A     ;Happens once per quarter second
	LD	HL,1
	LD	A,1	;set to repeating
	CALL	REQUEST_SIGNAL
	LD	(TickTimer),A		;Happens once per tick
    RET
;
;   Seed Random numbers
;   Seed in HL
SEED_RANDOM:
    LD (SEED),HL
    RR H
    RL L
    LD (SEED+2),HL
    RET
;
;   Generate a random number, based on the initial Seed
;   value.
;
RND:
    PUSH HL
	PUSH BC
	PUSH DE
	LD DE,(SEED+2)
	LD HL,(SEED)
	LD B,5
RLP1:
    RR H
	RL L
	RR D
	RL E
	DJNZ RLP1
	LD B,3
RLP2:
    PUSH DE
	LD DE,(SEED)
	OR A
	SBC HL,DE
	EX DE,HL
	POP HL
	DJNZ RLP2
	LD (SEED),HL
	LD (SEED+2),DE
	LD A,E
	;OR H
	POP DE
	POP BC
	POP HL
	RET
;
; NMI routine
;
NMI_Handler:
;	push hl
;	push af
	
;	ld 	 hl,PauseFlags
;	bit  PauseFlag_Disabled, (hl)
;	jr 	 nz, .skip

;	ld 	 a,($c041)
;	rlca
;	jr 	 c, +
;	set  0, (hl)		
;	ld 	 hl,$c043
;	ld 	 a,$04
;	xor  (hl)
;	ld 	 (hl),a

;.skip:
;	pop  af			
;	pop  hl			
	retn		

;
; Interrupt routine
;
Interrupt:
	push af
	push bc
	push de
	push hl
	exx
	ex af, af'
	push af
	push bc
	push de
	push hl
	push ix
	push iy

	in a, (CTRL_PORT)

	; update our time counter
    LD HL,(TIME)
    DEC HL
    LD (TIME),HL
    ;Now we can safely call any OS7 calls
    LD A,(SKIPMUSIC)
    CP 0
    JP NZ,NMI3
	CALL	PLAY_SONGS	;Update active music
	CALL	SND_MANAGER	;Prepare for next go at music
NMI3:
	; write sprite table
    CALL    SPRWRT
    LD A,(VDU_HOOK)
    CP 0cdh
    JR NZ,NMI2
    CALL VDU_HOOK
 NMI2:
	CALL	TIME_MGR

;Now restore everything
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	exx
	ex af, af'
	pop hl
	pop de
	pop bc
	pop af

    EI
	RET ;Non maskable interrupt used for:
		;music, processing timers, sprite motion processing

;
; Timer Routines
;

DONE:    EQU 7
REPEATCOUNT:  EQU 6
FREE:    EQU 5
EOT:     EQU 4
LONG:    EQU 3
AMERICA: DB 50

TIME_MGR:
    ld HL,(TIMER_TABLE_BASE)
NEXT_TIMER0:
    bit FREE,(HL)
    call z,DCR_TIMER
    bit EOT,(HL)
    jr nz,SCRAM
    inc HL
    inc HL
    inc HL
    jr NEXT_TIMER0
SCRAM:
    ret
;
DCR_TIMER:
    push HL
    bit LONG,(HL)
    jr z,DCR_S_MODE_TBL
    bit REPEATCOUNT,(HL)
    jr nz,DCR_L_RPT_TBL
DCR_L_MODE_TBL:
    inc HL
    ld E,(HL)
    inc HL
    ld D,(HL)
    dec DE
    ld A,E
    or D
    jr nz,SAVE_2_BYTES
    pop HL
    push HL
    jr SET_DONE_BIT
;
DCR_L_RPT_TBL:
    inc HL
    ld E,(HL)
    inc HL
    ld D,(HL)
    ex de,hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    dec de
    ld a,e
    or d
    jr nz,SAVE_2_BYTES
    inc hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    dec hl
    dec hl
    ld (hl),d
    dec hl
    ld (hl),e
    pop hl
    push hl
    jr SET_DONE_BIT
;
DCR_S_MODE_TBL:
    inc hl
    dec (hl)
    jr nz,TIMER_EXIT
    pop hl
    push hl
    bit REPEATCOUNT,(hl)
    jr z,SET_DONE_BIT
    inc hl
    inc hl
    ld a,(hl)
    dec hl
    ld (hl),a
    dec hl
    pop hl
    push hl
SET_DONE_BIT:
    set DONE,(hl)
TIMER_EXIT:
    pop hl
    ret
;
SAVE_2_BYTES:
    ld (hl),d
    dec hl
    ld (hl),e
    jr TIMER_EXIT
;
; Procedure Init Timer
; HL has address of Timer table
; DE has address of Timer Data table
;
INIT_TIMER_PARAM:
    dw 00002h
    dw 00002h
    dw 00002h
;
INIT_TIMER:
    ld (TIMER_TABLE_BASE),hl
    ld (hl),030h
    ex de,hl
    ld (NEXT_TIMER_DATA_BYTE),hl
    ret
;
FREE_SIGNAL:
    ld c,a
    ld hl,(TIMER_TABLE_BASE)
    ld b,a
    ld de,00003h
    or a
    jr z,FREE_MATCH
FREE1:
    bit EOT,(hl)
    jr nz,FREE_EXIT
    add hl,de
    dec c
    jr nz,FREE1
FREE_MATCH:
    bit FREE,(hl)
    jr nz,FREE_SET
    set FREE,(hl)
    bit REPEATCOUNT,(hl)
    jr z,FREE_SET
    bit LONG,(hl)
    jr z,FREE_SET
; FREE (DELETE) COUNTER
; Counter address in DE
FREE_COUNTER:
    inc hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    push de
    ld hl,(TIMER_TABLE_BASE)
    push hl
NEXT:
    bit EOT,(hl)
    jr nz,MOVE_IT
    bit FREE,(hl)
    jr nz,GET_NEXT
    ld a,(hl)
    and 048h
    cp 048h
    jr nz,GET_NEXT
    inc hl
    inc hl
    ld a,(hl)
    cp d
    jr c,GET_NEXT
    jr nz,SUBSTRACT_4
    dec hl
    ld a,(hl)
    cp e
    jr c,GET_NEXT
    jr z,FREE_EXIT
    inc hl
SUBSTRACT_4:
    ld d,(hl)
    dec hl
    ld e,(hl)
    dec de
    dec de
    dec de
    dec de
    ld (hl),e
    inc hl
    ld (hl),d
    ;jr GET_NEXT
;
GET_NEXT:
    pop hl
    inc hl
    inc hl
    inc hl
    push hl
    jr NEXT
;
MOVE_IT:
    ld b,000h
    or a
    pop hl
    pop de
    push hl
    ld hl,(NEXT_TIMER_DATA_BYTE)
    sbc hl,de
    ld c,l
    ld l,e
    ld h,d
    inc hl
    inc hl
    inc hl
    inc hl
    ldir
    ld bc,00008h
    sbc hl,bc
    ld (NEXT_TIMER_DATA_BYTE),hl
    pop hl
FREE_SET:
FREE_EXIT:
    ret
;
REQUEST_SIGNAL:
    ld c,a
    ex de,hl
    ld hl,(TIMER_TABLE_BASE)
    xor a
    ld b,a
TIMER1:
    bit FREE,(hl)
    jr z,NEXT_TIMER1
    push hl
    ld a,(hl)
    and 010h
    or 020h
    ld (hl),a
    xor a
    or d
    jr nz,LONG_TIMER
    or c
    jr z,NOT_A_REPEAT_TIMER
    set REPEATCOUNT,(hl)
NOT_A_REPEAT_TIMER:
    inc hl
    ld (hl),e
    inc hl
    ld (hl),e
    jr INIT_TIMER_EXIT
;
LONG_TIMER:
    set LONG,(hl)
    ld a,c
    or a
    jr z,NOT_A_LONG_REPEAT
    push de
    ex de,hl
    ld hl,(NEXT_TIMER_DATA_BYTE)
    ex de,hl
    set REPEATCOUNT,(hl)
    inc hl
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    pop de
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl
    ld (NEXT_TIMER_DATA_BYTE),hl
    jr INIT_TIMER_EXIT
;
NOT_A_LONG_REPEAT:
    inc hl
TIMER2:
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl
    jr INIT_TIMER_EXIT
;
NEXT_TIMER1:
    bit EOT,(hl)
    jr nz,MAKE_NEW_TIMER
    inc hl
    inc hl
    inc hl
    inc b
    JR TIMER1
;
MAKE_NEW_TIMER:
    push de
    push hl
    inc hl
    inc hl
    inc hl
    inc b
    ld (hl),030h
    ex de,hl
    pop hl
    res EOT,(hl)
    ex de,hl
    pop de
    jr TIMER1
;
INIT_TIMER_EXIT:
    pop hl
    res FREE,(hl)
    ld a,b
    ret
;
; Procedure Test Signal
; A has the Signal number to be tested
; A value of True(1) or False(0) is returned in A
;
TEST_SIGNAL:
    ld c,a
    ld hl,(TIMER_TABLE_BASE)
    ld b,a
    ld de,00003h
    or a
    jr z,SIGNAL_MATCH
TEST1:
    bit EOT,(hl)
    jr nz,SIGNAL_FALSE
    add hl,de
    dec c
    jr nz,TEST1
SIGNAL_MATCH:
    bit FREE,(hl)
    jr nz,SIGNAL_FALSE
    bit DONE,(hl)
    jr nz,SIGNAL_TRUE
SIGNAL_FALSE:
    xor a
    jr TEST_EXIT
;
SIGNAL_TRUE:
    bit REPEATCOUNT,(hl)
    jr nz,SIGNAL_TRUE1
    set FREE,(hl)
SIGNAL_TRUE1:
    res DONE,(hl)
    ld a,001h
TEST_EXIT:
    or a
    ret

VDP_INIT_DELAY:			; close to 1000 ms
	ld b, 11			; Loretta waits for 1903 ms, Gulkave waits for 536 ms

	ld de, $FFFF
.loop:
	ld hl, $39DE
.loop2:
	add hl, de
	jr c, .loop2
	
	djnz .loop
	ret

;NLEN:       EQU 005H

;**************************
;* FREQ_SWEEP *
;**************************
;.COMMENT }
;See User's Manual for description
;RETs Z SET: if note over
;RETs Z RESET: if sweep in progress or note not over
;}
FREQ_SWEEP:
    ; * if freq not swept, dec NLEN and RET [setting Z flag]
    ld a,(ix+FSTEP) ;check for no sweep code
    cp 000H ;set Z flag if FSTEP = 0
    ; if [psw,is,zero] ;note not to be swept
    jr nz,L20
    ld a,(ix+NLEN) ;dec NLEN and
    dec a ;SET Z flag if NLEN = 0
    ret z ;leave if note over with Z flag SET
    ld (ix+NLEN),a ;store decremented NLEN
    ret ;RET with Z flag RESET [note not over]
; * sweep going, so decrement FPSV
L20:
    push ix ;point HL to FPSV
    pop hl
    ld e,FPSV
    ld d,000H
    add hl,de
    call DECLSN ;decrement FPSV
    ; if [psw,is,zero] ;FPSV has timed out
    jr nz,L21
    ; * decrement NLEN and leave if sweep is over
    call MSNTOLSN ;reload FPSV from FPS
    dec hl ;point to NLEN [# steps in the sweep]
    ld a,(hl) ;decrement NLEN and
    dec a ;SET Z flag if NLEN = 0
    ret z ;leave if sweep over with Z flag set
    ; * sweep not over, so add FSTEP to FREQ
    ld (hl),a ;store decremented NLEN
    dec hl ;point HL to FREQ
    dec hl
    ld a,(ix+007H) ;A = FSTEP [two's complement step size]
    call ADD816 ;FREQ = FREQ + FSTEP
    inc hl ;point HL to hi FREQ
    res 2,(hl) ;RESET B2 in hi FREQ in case add cased > 10 bit
    or 0FFH ;RESET Z flag, sweep not over yet
L21:
    ret

;**************************
;* ATN_SWEEP *
;**************************
;.COMMENT }
;See User's Manual for description
;RETs Z SET: if byte 8 is 0 [means sweep is over, or note was never swept]
;RETs Z RESET: if sweep in progress
;}
ATN_SWEEP:
    ; * RET with Z SET if byte 8 = 00
    ld a,(ix+008H) ;check byte 8 for no sweep code
    cp 000H ;Z is set if byte 8=0
    ret z ;leave if Z set, sweep not going
    ; * sweep going, so dec APSV
    push ix ;point HL to APSV
    pop hl
    ld d,000H
    ld e,APSV
    add hl,de
    call DECLSN ;dec APSV [LSN of byte 9]
    ; if [psw,is,zero] ;APSV has timed out
    jr nz,L22
    ; * decrement ALEN to see if sweep over
    call MSNTOLSN ;reload APSV from APS
    dec hl ;point to ALEN [# of steps in the sweep]
    call DECLSN ;dec ALEN [LSN byte 8]
    jr z,L23
    ; * add ASTEP to ATN
    ld a,(hl) ;MSN A = ASTEP
    and 0F0H ;mask LSN
    ld e,a ;E = ASTEP | 0
    dec hl ;point HL to ATN
    dec hl
    dec hl
    dec hl
    ld a,(hl) ;MSN A = ATN
    and 0F0H ;A = ATN | 0
    add a,e ;MSN A = [ASTEP + ATN] | 0
    ld e,a ;Saved in E
    ld a,(hl) ;A = ATN | freq or CTRL
    and 00FH ;mask old ATN A = 0 | freq or CTRL
    or e ;OR in new ATN
    ld (hl),a ;store updated value back into song data area
    or 0FFH ;RESET Z flag, sweep not over yet
    jr L22
    ; ELSE
L23:
    ld (hl),000H ;set byte 8 to 0 to indicate end sweep
L22:
    ret

;**************************
;* UPATNCTRL *
;**************************
;.COMMENT }
;Perform single byte update of the snd chip noise control register or any
;attanuation register. IX is passed pointing to byte 0 of a song data area, MSN
;register C = formatted channel attenuation code.
;}
UPATNCTRL:
    ld a,(ix+004H) ;MSN A=ATN, LSN may be CTRL data
    bit 4,c ;test for ATN
    ; if [psw,is,nzero] ;ATN is to be sent, move it to LSN
    jr z,L24
    rrca ;swap nibbles
    rrca
    rrca
    rrca
L24:
    and 00FH ;mask MSN
    or c ;a = formatted register# | ATN or CTRL
    out (PSG),a ;output ATN or CTRL data
    ret

;
;**************************
;* UPFREQ *
;**************************
;.COMMENT }
;Perform double byte update of a sound chip frequency register. IX is passed
;pointing to byte0 of a song data area, MSN register D = formatted channel
;frequency code.
;}
UPFREQ:
    ld a,(ix+FREQ) ;A = F2 F3 F4 F5 F6 F7 F8 F9
    and 00FH ;A = 0 0 0 0 F6 F7 F8 F9
    or d ;A = FORMATTED REG# | F6 F7 F8 F9
    out (PSG),a ;output 1st freq byte
    ld a,(ix+FREQ) ;A = F2 F3 F4 F5 F6 F7 F8 F9
    and 0F0H ;A = F2 F3 F4 F5 0 0 0 0
    ld d,a ;save in D
    ld a,(ix+FREQ+1) ;LSN A = 0 0 F0 F1
    and 00FH ;A = 0 0 0 0 0 0 F0 F1
    or d ;A = F2 F3 F4 F5 0 0 F0 F1
    rrca ;swap nibbles
    rrca
    rrca
    rrca ;A = 0 0 F0 F1 F2 F3 F4 F5
    out (PSG),a ;output 2nd [most significant] freq byte
    ret

;
;**************************
;* DECLSN *
;**************************
;.COMMENT }
;Without affecting the MSN, decrement the LSN of the byte pointed to by HL.
;HL remains the same.
;RET with Z flag set if dec LSN results in 0, reset otherwise.
;RET with C flag set if dec LSN results in -1, reset otherwise.
;}
DECLSN:
    ld a,000H
    rrd ;A = 0 | LSN [HL]
    sub 001H ;Z flag set if dec to 0, C flag if dec to -1
    push af ;save Z and C flag
    rld ;[HL] = old MSN | new LSN
    pop af ;restore Z and C flags, A = 0 | new LSN
    ret

;
;**************************
;* DECMSN *
;**************************
;.COMMENT }
;Without affecting the LSN, decrement the MSN of the byte pointed to by HL.
;HL remains the same.
;RET with Z flag set if dec MSN results in 0, reset otherwise.
;RET with C flag set if dec MSN results in -1, reset otherwise.
;}
DECMSN:
    ld a,000H
    rld ;A = 0 | MSN [HL]
    sub 001H ;Z flag set if dec to 0, C flag if dec to -1
    push af ;save Z and C flag
    rrd ;[HL] = new MSN | old LSN
    pop af ;restore Z and C flags, A = 0 | new MSN
    ret

;
;**************************
;* MSNTOLSN *
;**************************
;.COMMENT }
;Copy MSN of the byte pointed to by HL to the LSN of that byte.
;HL remains the same.
;}
MSNTOLSN:
    ld a,(hl) ;A = MSN | LSN to be changed
    and 0F0H ;A = MSN | 0
    ld b,a ;save in B
    rrca ;swap nibbles
    rrca
    rrca
    rrca ;A = 0 | MSN
    or b ;A = MSN | MSN
    ld (hl),a ;[HL] = MSN | MSN
    ret

;
;**************************
;* ADD816 *
;**************************
;.COMMENT }
;Adds 8 bit two's complement signed value passed in A to the 16 bit location
;pointed to by HL.
;}
ADD816:
    ld b,000H ;set B for positive value in A
    bit 7,a ;if A is positive
    jr z,POS ;skip
    ld b,0FFH ;A is neg: extend sign bit thru B
POS:
    add a,(hl) ;do 8 bit add [and set Carry]
    ld (hl),a ;store result into LSN 16 bits number
    inc hl ;put MSB
    ld a,(hl) ;into A
    adc a,b ;A = MSB + Carry + B [B is 0 or FF]
    ld (hl),a ;store result into MSN
    dec hl ;re-point HL to LSB 16 bit number
    ret

;
;**************************
;* PT_IX_TO_SxDATA *
;**************************
;.COMMENT }
;SONGNO passed in B.
;Point IX to byte 0 in SONGNO's song data area.
;RET with both DE and IX pointing to SxDATA,
;HL pointing to MSB SxDATA entry in LST_OF_SND_ADDRS.
;}
PT_IX_TO_SxDATA:
    ; * IX & DE := addr of byte 0 in SONGNO's song data area,
    ; ; HL pointing to MSB SxDATA entry in LST_OF_SND_ADDRS.
    ;point HL to start LST_OF_SND_ADDRS
    ld hl,(PTR_TO_LST_OF_SND_ADDRS)
    dec hl ;init HL for addition
    dec hl
    ld c,b ;from 4*SONGNO in C
    ld b,000H
    rlc c
    rlc c
    add hl,bc ;HL pts to SxDATA's entry in LST_OF_SND_ADDRS
    ld e,(hl) ;move addr SxDATA to IX thry DE
    inc hl
    ld d,(hl)
    push de
    pop ix
    ret

;
;**************************
;* LEAVE_EFFECT *
;**************************
;.COMMENT }
;LEAVE_EFFECT, called by a special sound effect routine when it's finished,
;restores the SONGNO of song to which the effect note belongs to B5-B0 of
;byte 0 in the effect's data area, and loads bytes 1 and 2 with the address of
;the next note in the song. The address of the 1 byte SONGNO (saved by the
;effect when 1st called) is passed in DE. The 2 byte address of the next note
;in the song, also saved by the effect, is passed in HL. IX is assumed to be
;pointing to byte 0 of the data area to which the song number is to be
;restored. Bits 7 and 6 of the saved SONGNO are ignored, and therefore may be
;used by the effect to store flag information during the course of the note.
;}
LEAVE_EFFECT:
    ld (ix+001H),l ;LSB NEXT_NOTE_PTR := LSB addr next note in song
    ld (ix+002H),h ;MSB NEXT_NOTE_PTR := MSB addr next note in song
    ld a,(de) ;A := x x SONGNO (i.e., the saved, original SONGNO)
    and 03FH ;A := 0 0 SONGNO
    ld b,a ;Saved in B
    ld a,(ix+000H) ;A := CH# | 62 (all effect notes have SONGNO = 62)
    and 0C0H ;A := CH# 0 0 0 0 0 0
    or b ;A := CH# | SONGNO
    ld (ix+000H),a ;restore song number
    ret

;
;**************************
;* AREA_SONG_IS *
;**************************
;.COMMENT }
;The address of byte 0 of a song data area is passed in IX. The song # of
;the song using that area is returned in A [0FFH if inactive]. If a special
;effect was using that area, 62 is returned in A and HL is returned with the
;address of the special sound effect routine.
;}
AREA_SONG_IS:
    ld a,(ix+000H) ;A := CH# | SONGNO or 62, or A := FF
    cp 0FFH
    ret z ;leave if A = FF (area inactive)
    and 03FH ;mask CH#
    cp 03EH
    ret nz ;leave with A = SONGNO (not a special effect)
    push ix ;point HL to byte 1
    pop hl
    inc hl
    ld e,(hl) ;save LSB effect addr in E
    inc hl ;HL to byte 2
    ld d,(hl) ;save MSB effect addr in D
    ex de,hl ;HL := addr special effect
    ret

;**************************
;* INIT_SOUND *
;**************************
;.COMMENT }
;see Users' Manual for description; includes ENTRY POINT ALL_OFF
;addr LST_OF_SND_ADDRS passed in HL
;n = # of song data areas to init, passed in B
;}
; #Defines
SR1ATN: EQU 090H
SR2ATN: EQU 0B0H
SR3ATN: EQU 0D0H
SRNATN: EQU 0F0H
SR1FRQ: EQU 080H
SR2FRQ: EQU 0A0H
SR3FRQ: EQU 0C0H
SRNCTL: EQU 0E0H

INIT_SOUND:
    ; * initialize PTR_TO_LST_OF_SND_ADDRS with value passed in HL
    ld (PTR_TO_LST_OF_SND_ADDRS),hl
    ; * store inactive code at byte 0 of each of the n data areas [B=n]
    inc hl ;pt HL to song 1 data area entry in LST_OF_SND_ADDRS
    inc hl
    ld e,(hl) ;pt DE to byte 0 in first song data area
    inc hl
    ld d,(hl)
    ex de,hl ;pt HL to byte 0 in first song data area
    ld e,00AH ;set DE for 10 byte increment
    ld d,000H
B1:
    ld (hl),0FFH ;deactivate area
    add hl,de ;pt HL to byte 0 next area (10 bytes away)
    djnz B1 ;do this for n (passed in B) data areas
; * store end of data area code (0) at 1st byte after last song data area
    ld (hl),000H ;store end of data area code in byte 0 data area n+1
; * set the 4 channel data area pointers to a dummy, inactive data area
    ld hl,DUMAREA ;point HL to inactive byte below [after the RET]
    ld (PTR_TO_S_ON_0),hl ;store addr DUMAREA at PTR_TO_S_ON_0
    ld (PTR_TO_S_ON_1),hl ;store addr DUMAREA at PTR_TO_S_ON_1
    ld (PTR_TO_S_ON_2),hl ;store addr DUMAREA at PTR_TO_S_ON_2
    ld (PTR_TO_S_ON_3),hl ;store addr DUMAREA at PTR_TO_S_ON_3
; * initialize SAVE_CTRL
    ld a,0FFH           ;note: this is only time MSN SAVE_CTRL
                        ; will be non zero,
    ld (SAVE_CTRL),a    ;thus ensuring PLAY_SONGS will output
                        ; 1st real CTRL data
ALL_OFF:
; * turn off all 4 sound generators
    ld a,SR1ATN+OFF ;form off code for tone generator 1
    out (PSG),a ;send it out
    ld a,SR2ATN+OFF ;form off code for tone generator 2
    out (PSG),a ;send it out
    ld a,SR3ATN+OFF ;form off code for tone generator 3
    out (PSG),a ;send it out
    ld a,SRNATN+OFF ;form off code for noise generator, N
    out (PSG),a ;send it out
    ret

;
DUMAREA:
    db 0FFH

;**************************
;* JUKE_BOX *
;**************************
;.COMMENT }
;see Users' Manual for description
;SONGNO passed in B
;}
PLAY_IT:
JUKE_BOX:
; * RET if song already in progress
    push bc ;save SONGNO on stack
    call PT_IX_TO_SxDATA ;point IX to SONGNO's song data area
    ld a,(ix+000H) ;A := CH# [if any] | SONGNO [if any]
    and 03FH ;A := 0 0 SONGNO
    pop bc ;B := SONGNO
    cp b ;test if already in progress
    ret z ;if so, leave
; * load 1st note and set NEXT_NOTE_PTR [thru LOAD_NEXT_NOTE]
    ld (ix+000H),b ;store SONGNO in byte 0
    dec hl      ;-HL left by PT_IX_TO_SxDATA
                ; pointing to MSB SxDATA
    dec hl      ;-entry in LST_OF_SND_ADDRS; point HL to note list
    ld d,(hl)   ;-starting addr entry in LST_OF_SND_ADDRS
                ; and save this
    dec hl      ;-addr in DE
    ld e,(hl)   ;DE now has the initial value for NEXT_NOTE_PTR
    ld (ix+001H),e ;set NEXT_NOTE_PTR for 1st note in song
    ld (ix+002H),d
    call LOAD_NEXT_NOTE ;load note, byte 0 := CH#|SONGNO,
                        ; set new NEXT_NOTE_PTR
    call UP_CH_DATA_PTRS ;new song, so update channel data ptrs
    ret

;**************************
;* SONG_MANAGER *
;**************************
;.COMMENT }
;see Users' Manual for description
;}
SND_MANAGER:
; * IX := addr of song #1 data area [S1DATA]
    ld b,001H ;pt IX to byte 0 song data area for song# 1
    call PT_IX_TO_SxDATA
; LOOP until end of song data areas
L1:
    ld a,ENDSDATA ;check for end of song data areas
    cp (ix+000H) ;set Z flag if negative
    ret z ;leave [Z set],
; if all data areas have been processed
; * process active song data areas
    call PROCESS_DATA_AREA ;update counters of call effect; get next note
; * point IX to byte 0 next song data area
    ld e,00AH
    ld d,000H
    add ix,de
    jr L1 ; repeat loop
;**************************
;* UP_CH_DATA_PTRS *
;**************************
;.COMMENT }
;For each active data area, starting with S1DATA and proceeding in order, load
;the associated channel data area pointer [PTR_TO_S_ON_x] with the address of
;byte 0. This routine is called by JUKE_BOX, when a song starts and
;PROCESS_DATA_AREA when the channel using a data area has changed as a result
;of calling LOAD_NEXT_NOTE [this happens when a song finishes and when it
;switches back and forth between noise and tone notes].
;}
UP_CH_DATA_PTRS:
    push ix ;save curent IX
    ld hl,DUMAREA ;set all 4 ch data ptrs to dummy inactive area
    ld (PTR_TO_S_ON_0),hl
    ld (PTR_TO_S_ON_1),hl
    ld (PTR_TO_S_ON_2),hl
    ld (PTR_TO_S_ON_3),hl
    ld b,001H
    call PT_IX_TO_SxDATA
; LOOP until end of song data areas
L2:
    ld a,(ix+000H)
    cp ENDSDATA ;check for end of song data areas
    jr z,DONE_SNDMAN ;leave loop if all data areas checked
; * if area active, set appropriate channel data area pointer
    cp INACTIVE ;check for inactive data area:
                ; don't up date ptr if so
; if [PSW,IS,ZERO] ;area is active: update channel data ptrs
    jr z,L9
    ld a,(ix+000H) ;get CH# in A
    and 0C0H ;B7 - B6 in A = CH#
    rlca ;form CH# * 2 in A,i.e., the offset from
    rlca ;PTR_TO_S_ON0 of channel data area pointer
    rlca ;that points to channel CH#
    ld e,a ;add offset to addr of PTR_TO_C_ON_0
    ld d,000H
    ld hl,PTR_TO_S_ON_0
    add hl,de ;HL points to proper channel data area pointer
    push ix ;store this song data area's byte 0 addr there
    pop de
    ld (hl),e
    inc hl
    ld (hl),d
; * point IX to byte 0 next song data area
L9:
    ld e,00AH
    ld d,000H
    add ix,de
    jr L2 ;repeat loop
;
DONE_SNDMAN:
    pop ix ;restore IX
    ret
;
;**************************
;* UP_CH_DATA_PTRS *
;**************************
;.COMMENT }
;See Users' Manual for description
;Terminology: SFX = address of sound effect routine
;}
PROCESS_DATA_AREA:
    call AREA_SONG_IS ;return area's SONGNO in A [and addr SFX in HL]
    cp INACTIVE ;test for inactive code
    ret z ;RET, no processing if area inactive
; * if special effect, call it to process the data area
    cp 03EH ;test for special sound effect
    jr nz,L10
    ld e,007H ;pt HL to SFX+7, starting adr of the effect's code
    ld d,000H
    add hl,de
    jp (hl) ;do 1 pass thru effect, RET from effect
;
; * else process a non-effect note
L10:
    call ATN_SWEEP ;process atn sweep data, if any
    call FREQ_SWEEP ;proc frq sweep data, if any, & note dura timers
; if [psw,is,zero] ;note is over
    jr nz,L12
EFXOVER:
    ld a,(ix+000H) ;A := CH# | SONGNO this note    
    push af ;save on stack
    call LOAD_NEXT_NOTE ;load data for next note
    pop bc ;B := CH# | SONGNO previous note
    ld a,(ix+000H) ;A := CH# | SONGNO new note [may be inactive]
    cp b ;check against new note's CH# | SONGNO
; if [psw,is,nzero] ;change to/from tone/efx/noise
    jr z,L12
    call UP_CH_DATA_PTRS ;to maintain data area priority system
L12:
    ret

;**************************
;* PLAY_SONGS_ *
;**************************
;.COMMENT }
;see Users' Manual for description
;SFX refers to the beginning address of a special sound effect routine
;}
PLAY_SONGS:
; * output CH1 attenuation and frequency
    ld a,SR1ATN+OFF ;format CH1 OFF byte into A
    ld c,SR1ATN ;format MSN C for CH1 attenuation
    ld d,SR1FRQ ;format MSN D for CH1 frequency
    ld ix,(PTR_TO_S_ON_1) ;point IX to byte 0 data area
    ; of song for CH1
    call TONE_OUT
    ; * output CH2 attenuation and frequency
    ld a,SR2ATN+OFF ;format CH2 OFF byte into A
    ld c,SR2ATN ;format MSN C for CH2 attenuation
    ld d,SR2FRQ ;format MSN D for CH2 frequency
    ld ix,(PTR_TO_S_ON_2) ;point IX to byte 0 data area
    ; of song for CH2
    call TONE_OUT
    ; * output CH3 attenuation and frequency
    ld a,SR3ATN+OFF ;format CH3 OFF byte into A
    ld c,SR3ATN ;format MSN C for CH3 attenuation
    ld d,SR3FRQ ;format MSN D for CH3 frequency
    ld ix,(PTR_TO_S_ON_3) ;point IX to byte 0 data area
    ; of song for CH3
    call TONE_OUT
    ; * output CH0 [noise] ATN [and CTRL, if different from last time]
    ld a,SRNATN+OFF ;format CH0 OFF byte into A
    ld c,SRNATN ;format MSN C for CH0 attenuation
    ld ix,(PTR_TO_S_ON_0) ;point IX to byte 0 data area
    ; of song for CH0
    ld e,(ix+000H) ;look for inactive code 0FFH
    inc e ;this sets Z flag if E = 0FFH
    ; if [psw,is,zero] ; song data area is inactive
    jr nz,L5
    out (PSG),a ;turn off CH0
    jr L6
; ELSE
L5:
    call UPATNCTRL ;send out current ATN
    ld a,(ix+004H) ;LSN A = current CTRL data
    and 00FH ;mask MSN
    ld hl,SAVE_CTRL ;point to last CTRL data sent
    cp (hl) ;compare
    ; if [psw,is,nzero] ;CTRL has changed
    jr z,L6
    ld (hl),a ;SAVE_CTRL = new CTRL data
    ld c,SRNCTL ;send new CTRL data
    call UPATNCTRL
L6:
    ret
;
TONE_OUT:
    ld e,(ix+000H) ;look for inactive code, oFFH
    inc e ;this sets Z flag if E = 0FFH
    ; if [psw,is,zero] ;song data area is inactive
    jr nz,L7
    out (PSG),a ;turn off CHx
    jr L8
; ELSE ;send out current ATN and FREQ
L7:
    call UPATNCTRL ;send out attenuation
    call UPFREQ ;send out frequency
L8:
    ret

;**************************
;* LOAD_NEXT_NOTE *
;**************************
;.COMMENT }
;see Users' Manual for description
;SFX refers to the beginning address of a special sound effect routine
;}
; #Defines
;ATN EQU 4
;NLEN EQU 5
;FSTEP EQU 7
;ASTEP EQU 8
;INACTIVE EQU 0FFH
LOAD_NEXT_NOTE:
; * deactivate area, save SONGNO on stack
    ld a,(ix+000H) ;A := byte 0
    and 03FH ;mask CH#, if any
    push af ;save SONGNO on stack
    ld (ix+000H), INACTIVE ;deactivate area
    ; A := header new note
    ld l,(ix+001H) ;HL := addr new note in ROM
    ld h,(ix+002H)
    ld a,(hl) ;A := header new note
    ; * save header of new note in song on stack
    ; and load its data CASE note type
    ld b,a ;save header new not in B
    ; - test for rest
    bit 5,a ;test for rest
    ; if [psw,is,nzero] ;note is rest
    jr z,L13
; --CASE-- rest
REST:
    push bc ;save header on stack
    and 01FH ;mask all but duration bits
    inc hl ;HL = addr of the header of the note after this note
    ld (ix+001H),l ;store in NEXT_NOTE_PTR
    ld (ix+002H),h
    ; move this note's data and fill in bytes where necessary
    ld (ix+ATN),0F0H ;set stn off
    ld (ix+NLEN),a ;NLEN := 5 bit duration
    ld (ix+FSTEP),000H ;indicate freq not to be swept
    ld (ix+ASTEP),000H ;indicate atn not to be swept
    jp MODB0
;
; - test for end of song
L13:
    bit 4,a ;test for end
    ; if [psw,is,nzero] ;end of song
    jr z,L14
    bit 3,a ;test for repeat
    ; if [PSW,is,nzero] ;end of song
    jr z,ENDNOREP
; --CASE-- end song, repeat
ENDREP:
    pop bc ;B := SONGNO
    call JUKE_BOX ;to reload 1st note of this song
    ret ;to PROCESS_DATA_AREA, don't save header
;
; --CASE-- end song, no repeat
ENDNOREP:
    ld a, INACTIVE
    push af ;save inactive code to end song
    jp MODB0 ;to load byte 0
;
; - test for special sound effect
L14:
    and 03CH ;mask irrelevant bits
    cp 004H ;test for B5 - B2 = 0001
    ; if [psw,is,zero] ;note is a special effect
    jr nz,L15
; --CASE-- special effect
EFFECT:
    pop iy ;IY := SONGNO
    push iy ;put SONGNO back on stack
    push bc ;save header on stack; NEXT_NOTE_PTR := SFX, DE := SFX
    inc hl ;-pt HL to next byte [LSB addr SFX]
    ld e,(hl) ;-E := LSB SFX
    ld (ix+001H),e ;-put LSB of SFX in byte 1 of SxDATA [NEXT_NOTE_PTR]
    inc hl ;-pt HL to NSB SFX
    ld d,(hl) ;-D := NSB SFX
    ld (ix+002H),d ;-put NSB SFX in byte 2 of SxDATA
    inc hl ;point HL to next note [after this new note]
    push iy ;A := SONGNO
    pop af
    push de ;PASS1 on the stack
    pop iy
    ld de,PASS1 ;create "CALL [IY]" with RET to PASS1 by storing
    push de ;PASS1 on stack
    jp (iy) ;1st 7 bytes SFX will save addr next note & SONGNO
;
PASS1:
    ld d,000H ; in same fashion, create a "CALL (IY+7)"
    ld e,007H ; to allow SFX to load initial values
    add iy,de
    ld de,MODB0 ;RET to MODB0
    push de
    jp (iy) ;INFO: index jump
; - if here, note is type 0 - 3
L15:
    push bc ; save header on stack
    ld a,b ; A := fresh copy header
    and 003H ; mask all but type number
    cp 000H ; test for type 0
    ; if [psw,is,zero] ; note is type 0: fixed freq and atn
    jr nz,L16
; --CASE-- note type 0
; * set up NEXT_NOTE_PTR
TYPE0:
    inc hl ; next note [after this new note] is 4 bytes away,
    inc hl ; point HL to it
    inc hl
    inc hl
    ld (ix+001H),l ; put addr in NEXT_NOTE_PTR
    ld (ix+002H),h
    ; move new note data and fill in bytes where necessary
    dec hl ; point HL back to 1st ROM data to move, NLEN
    ld de,00005H ; point DE to destination: bytes 5,4, and 3
    call DE_TO_DEST
    ld bc,00003H ; move 3 bytes
    lddr
    ld (ix+FSTEP),000H ; set for no freq sweep
    ld (ix+ASTEP),000H ; set for no atn sweep
    jr MODB0
;
L16:
    cp 001H ; test for type 1
    ; if [psw,is,zero] ; note is type 1: swept freq, fixed attenuation
    jr nz,L17
; --CASE-- not type 1
; * set up NEXT_NOTE_PTR
TYPE1:
    ld e,006H ; note after this note is 6 bytes away,
    ld d,000H ; pt HL to it
    add hl,de
    ld (ix+001H),l ; store in NEXT_NOTE_PTR
    ld (ix+002H),h
    ; move new note data and fill in bytes where necessary
    dec hl ; point HL back to 1st ROM data to move, FSTEP
    inc e ; E:=7; point DE to destination: bytes 7 - 3
    call DE_TO_DEST
    ld bc,00005H ; move 5 bytes
    lddr
    ld (ix+ASTEP),000H ; set for no atn sweep
    jr MODB0
;
L17:
    cp 002H ; test for type 2
    ; if [psw,is,zero] ; note is type 2: fixed freq, swept attenuation
    jr nz,TYPE3
; --CASE-- note type 2
; * set up NEXT_NOTE_PTR
TYPE2:
    ld e,006H ; pt HL to note after this note
    ; since it's 6 bytes away,
    ld d,000H ; pt HL to it by adding 6
    add hl,de
    pop af ; A := header this note [CH# |SONGNO]
    push af ; put back on stack
    and 0C0H ; mask SONGNO, leaving CH#
    ; if [psw,is,zero] ; This is a noise note,
    ; which is only 5 ROM bytes long
    jr nz,L18
    dec hl ; so move HL back 1 byte
L18:
    ld (ix+001H),l ; put addr in NEXT_NOTE_PTR
    ld (ix+002H),h
    ; move new note data and fill in bytes where necessary
    dec hl ; point HL back to 1st ROM data to move, APS
    ld e,009H ; point DE to destination: bytes 9,8,5 - 3
    call DE_TO_DEST
    ld bc,00002H ; move 2 bytes
    lddr ; when done, DE points to FSTEP, HL to ROM NLEN
    ld a,000H
    ld (de),a ; FSTEP := 0 for no freq sweep
    dec de ; pt DE to RAM NLEN
    dec de
    ld c,003H ; move last 3 ROM bytes
    ; if this is a noise note, garbage
    lddr ; will be loaded into byte 3, buts that's OK
    jr MODB0
;
TYPE3:
    ld e,008H ; note after this note is 8 bytes away
    ld d,000H ; pt HL to it
    add hl,de
    ld (ix+001H),l ; put addr in NEXT_NOTE_PTR
    ld (ix+002H),h
    ; move new note data and fill in bytes where necessary
    dec hl ; Point HL back to 1st ROM data to move, APS
    push ix ; Point DE to destination: bytes 9-3
    pop iy ; IY := Addr byte 0 [and DE = 6]
    ld e,009H ; DE := 9
    add iy,de ; IY := Addr byte 9 [APS]
    push iy
    pop de ; DE := addr APS
    ld bc,00007H ; move 7 bytes
    lddr
MODB0:
    push ix ;pt HL to byte 0
    pop hl
    pop af ; A := Header new note
    pop bc ; B := SONGNO
    cp 0FFH ; Test for inactive [song over, as detected above]
    ret z
    ld d,a ; Save header in D
    and 03FH ; Rid channel bits
    cp 004H ; Special effect
    jr nz,L20_LOAD_NEX
    ld b,03EH
L20_LOAD_NEX:
    ld a,d ; Restore A to header
    and 0C0H ; A := CH# 0 0 0 0 0 0
    or b ; A := new CH# | SONGNO
    ld (hl),a ; Store back in byte 0
L19:
    ret
;
DE_TO_DEST:
;DE passed = offset from byte 0, RETed with address byte offset
    push ix
    pop iy ; IY := Addr byte 0 (and DE = offset)
    add iy,de ; IY := Addr byte 0 + offset
    push iy
    pop de ; DE := Addr of destination byte in SxDATA
    ret


; Set origin in MTX RAM area
ORG $0000 
STACK:  EQU	$a000

TickTimer:    DS 1 ; Signal that 3 frames has elapsed
HalfSecTimer: DS 1 ; Signal that 1/2 second has elapsed
QtrSecTimer:  DS 1 ; Signal that 1/4 second has elapsed
EigthSecTimer:  DS 1 ; Signal that 1/8 second has elapsed
TIME:         DS 2
SEED:	      DS 4
MOVDLY:       DS 10      ; Up to 10 movement timers

; Sprite positions
SPRTBL:       DS 80h
SPRORDER:     DS 1 ; flag to indicate the current sprite write direction
TIMER_TABLE:	    DS 16	;Pointer to timers table (16 timers)
TIMER_DATA_BLOCK:	DS 58	;Pointer to timers table for long timers
                            ;4 bytes * 16 longer than 3 sec timers
VDU_HOOK: DS 4 ; NMI VDU Delayed writes hook

TIMER_LENGTH: DS 1
TEST_SIG_NUM: DS 1
TIMER_TABLE_BASE: DS 2
NEXT_TIMER_DATA_BYTE: DS 2

SKIPMUSIC: DS 1 ; skip music player when not zero

PAUSEFLAGS: DS 1
PAUSECOUNT: DS 1
JOYSTICKDATA: DS 2

; sound routines
PTR_TO_LST_OF_SND_ADDRS: DS 2
PTR_TO_S_ON_0: DS 2
PTR_TO_S_ON_1: DS 2
PTR_TO_S_ON_2: DS 2
PTR_TO_S_ON_3: DS 2
SAVE_CTRL: DS 2

joy1_data: DS 1
joy2_data: DS 1
key1_data: DS 1
key2_data: DS 1

RAMSTART: EQU $ ; Setup where game specific values can start

; RAM Usage: 30h+80h+1+16+58+4 = 255 bytes
