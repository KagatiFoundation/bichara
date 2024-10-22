#!/bin/sh

BUILD_PATH=../

gcc -c -Wall -Werror -fpic sock.c
gcc -shared -o $BUILD_PATH/libbich.so sock.o