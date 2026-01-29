@echo off

set c="C:\Users\tcrui\OneDrive\Emulation\Memotech MTX\memu\memu"
set c=%c% -iobyte 0x80
set c=%c% -addr 0x0100 -mem compression.com
set c=%c% -vid-win-big -vid-win-big -snd-portaudio
set c=%c% %*
%c%