@echo off

set c="C:\Users\tcrui\OneDrive\Emulation\Memotech MTX\memu\memu"
set c=%c% -iobyte 0x80
set c=%c% -addr 0x0100 -mem template.com
set c=%c% -vid-win-big -vid-win-big -snd-portaudio 
@rem -diag-ui-mem -diag-ui-dis
set c=%c% %*
%c%