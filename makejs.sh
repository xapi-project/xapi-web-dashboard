#!/bin/sh -e

cd _build/src
js_of_ocaml main.byte
ln -sf _build/src/main.js ../..
