#!/bin/sh

cabal build && ./dist/build/qvmgen/qvmgen -i ~/work/metaquake/src/game/quake/vm_builtins.c   -o builtins.txt   \
                                          -i ~/work/metaquake/src/game/quake/vm_extensions.c -o extensions.txt \
                                          -- -DMETAQUAKE_CODE_GENERATION             \
                                             -I/home/ftrvxmtrx/work/metaquake/build/ \
                                             -I/home/ftrvxmtrx/work/metaquake/src    \
                                             `pkg-config glib-2.0 --cflags`
