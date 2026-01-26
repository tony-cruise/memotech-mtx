rem Memotech MTX Template Make file (RASM x64 version)

@rem assemble source file %1%.asm to binary %1%.com

rasm_x64.exe %1.asm -oa -or -s

if exists %1.com del %1.com

if exists %1.bin ren %1.bin %1.com

