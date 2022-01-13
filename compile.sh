#!/usr/bin/env sh

file="build/out"

# set -xe
cat $@ | ./build/klx | ./build/opt > "$file.asm"
nasm -felf64 -o "$file.o" "$file.asm"
ld.lld -s -n --gc-sections --lto-O3 -O3 -o build/a.out "$file.o"
strip -R .note -R .comment -X -x -s build/a.out
