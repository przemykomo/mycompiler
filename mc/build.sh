#!/bin/sh
set -xe
cargo run -- test/test.mclang output.o
# nasm -felf64 output.asm -o output.o
gcc test/cfunc.c -c -ggdb -O0
#gcc -nostartfiles -no-pie output.o cfunc.o -o output
gcc -no-pie output.o cfunc.o -o output -ggdb
