;---------------------------------------------
; Memotech MTX ver 1.00 (C) Electric Adventures 2026
;---------------------------------------------

; Video Display Processor
DATA_PORT: EQU $01 ;
CTRL_PORT: EQU $02 ;


; Sound Generator
SND_PORT:  EQU $06 ; write only

; Keyboard ports
KBD_DATA: EQU $06 ; read only
KBD_CTRL: EQU $05 ; read/write


; Video modes
SCRMODE_STANDARD:               EQU 00h
SCRMODE_TEXT:                   EQU 10h
SCRMODE_MULTICOLOR:             EQU 08h
SCRMODE_BITMAP:                 EQU 02h
SCRMODE_BITMAP_TEXT:            EQU 12h
SCRMODE_BITMAP_MULTICOLOR:      EQU 0ah
SCRMODE_BITMAP_TEXT_MULTICOLOR: EQU 1ah

; TMS99xxA colours
COLOR_TRANSPARENT:          EQU 00h
COLOR_BLACK:                EQU 01h
COLOR_GREEN:                EQU 02h
COLOR_LIGHT_GREEN:          EQU 03h
COLOR_BLUE:                 EQU 04h
COLOR_LIGHT_BLUE:           EQU 05h
COLOR_DARK_RED:             EQU 06h
COLOR_CYAN:                 EQU 07h
COLOR_RED:                  EQU 08h
COLOR_LIGHT_RED:            EQU 09h
COLOR_YELLOW:               EQU 0ah
COLOR_LIGHT_YELLOW:         EQU 0bh
COLOR_DARK_GREEN:           EQU 0ch
COLOR_MAGENTA:              EQU 0dh
COLOR_GRAY:                 EQU 0eh
COLOR_WHITE:                EQU 0fh

; Any system values
; VRAM DEFAULT TABLES
VRAM_PATTERN:       EQU $0000
VRAM_NAME:          EQU $1800
VRAM_SPRATTR:       EQU $1B00
VRAM_COLOR:         EQU $2000
VRAM_SPRGEN:        EQU $3800
