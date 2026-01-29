@echo off

echo Memotech MTX Template Make file (RASM x64 or SJASMPlus version)

echo assemble source file %1.asm to binary %1.com

rem sjasmplus.exe %1.asm
rasm_x64.exe %1.asm -oa -or -s

echo Clean up intermediate files
IF EXIST "%1.com" DEL "%1.com"


echo Rename output .bin to .com
ren %1.bin %1.com

echo end of make.bat
