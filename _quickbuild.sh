#!/bin/sh

set -xe

wget -O raymath.h https://raw.githubusercontent.com/raysan5/raylib/f1007554a0a8145060797c0aa8169bdaf2c1c6b8/src/raymath.h
wget -O raylib.h https://raw.githubusercontent.com/raysan5/raylib/f1007554a0a8145060797c0aa8169bdaf2c1c6b8/src/raylib.h
wget -O libraylib.a https://pub.krzysckh.org/raylib-bin/libraylib5-linux64-opengl33.a

cc -o lager-quickbuild -I. lager.c -L. -l:libraylib5-linux64-opengl33.a -lm
