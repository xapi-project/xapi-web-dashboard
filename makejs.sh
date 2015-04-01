#!/bin/sh -e

cd _build/src
js_of_ocaml --pretty --noinline --debug-info main.byte
ln -sf _build/src/main.js ../..
