;---------------------------------------------
; Memotech MTX ver 1.00 (C) Electric Adventures 2026
;---------------------------------------------

; Video Display Processor
DATA_PORT  EQU $01 ;
CTRL_PORT EQU $02 ;


; Sound Generator
PSG  EQU $06 ; write only
OFF  EQU $0f ; [no sound]

; Special byte 0 codes
INACTIVE EQU $ff
SEFFECT EQU 62 ; special sound effect
ENDSDATA EQU 0

; Offsets within an SxDATA song data area
CH             EQU 0 ; channel
SONGNO         EQU 0 ; song number
NEXTNOTEPTR    EQU 1
FREQ           EQU 3 ; frequency
ATN            EQU 4 ; attenuation
CTRL           EQU 4
NLEN           EQU 5 ; noise
FPS            EQU 6 ; frequence sweep
FPSV           EQU 6
FSTEP          EQU 7
ALEN           EQU 8 ; attenuation sweep
ASTEP          EQU 8
APS            EQU 9
APSV           EQU 9
;
; Song end codes
CH0END         EQU $10
CH1END         EQU $50
CH2END         EQU $90
CH3END         EQU $D0
CH0REP         EQU $18
CH1REP         EQU $58
CH2REP         EQU $98
CH3REP         EQU $D8
;
; Channel numbers, B7-B6
CH0            EQU $00
CH1            EQU $40
CH2            EQU $80
CH3            EQU $C0

; Keyboard ports
KBD_DATA EQU $06 ; read only
KBD_CTRL EQU $05 ; read/write


; Video modes
SCRMODE_STANDARD               EQU 00h
SCRMODE_TEXT                   EQU 10h
SCRMODE_MULTICOLOR             EQU 08h
SCRMODE_BITMAP                 EQU 02h
SCRMODE_BITMAP_TEXT            EQU 12h
SCRMODE_BITMAP_MULTICOLOR      EQU 0ah
SCRMODE_BITMAP_TEXT_MULTICOLOR EQU 1ah

; TMS99xxA colours
COLOR_TRANSPARENT          EQU 00h
COLOR_BLACK                EQU 01h
COLOR_GREEN                EQU 02h
COLOR_LIGHT_GREEN          EQU 03h
COLOR_BLUE                 EQU 04h
COLOR_LIGHT_BLUE           EQU 05h
COLOR_DARK_RED             EQU 06h
COLOR_CYAN                 EQU 07h
COLOR_RED                  EQU 08h
COLOR_LIGHT_RED            EQU 09h
COLOR_YELLOW               EQU 0ah
COLOR_LIGHT_YELLOW         EQU 0bh
COLOR_DARK_GREEN           EQU 0ch
COLOR_MAGENTA              EQU 0dh
COLOR_GRAY                 EQU 0eh
COLOR_WHITE                EQU 0fh

; Any system values
; VRAM DEFAULT TABLES
VRAM_PATTERN       EQU $0000
VRAM_NAME          EQU $1800
VRAM_SPRATTR       EQU $1B00
VRAM_COLOR         EQU $2000
VRAM_SPRGEN        EQU $3800

; joystick data bits
JOY_UP    EQU 0
JOY_DOWN  EQU 2 
JOY_LEFT  EQU 3
JOY_RIGHT EQU 1
JOY_FIRE1 EQU 6
JOY_FIRE2 EQU 7