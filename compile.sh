#!/usr/bin/env sh

file="build/out"

# set -xe
./build/klx $@ > "$file.asm"
nasm -felf64 -o "$file.o" "$file.asm"
ld.lld --lto-O3 -O3 -o build/a.out "$file.o"
strip -R .note -R .comment -X -x -s build/a.out
