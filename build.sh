#!/bin/sh
cargo run -- test/test.mc output.asm
nasm -felf64 output.asm -o output.o
gcc test/cfunc.c -c
#gcc -nostartfiles -no-pie output.o cfunc.o -o output
gcc -no-pie output.o cfunc.o -o output
