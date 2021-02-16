#!/bin/sh -e

PREFIX="$1"

for pkg in bytes compiler-libs dynlink findlib graphics stdlib str threads unix; do
  cp -r "${PREFIX}/lib/${pkg}" "${PREFIX}/esp32-sysroot/lib/"
done
ln -sf "${PREFIX}/bin/ocamlrun" "${PREFIX}/esp32-sysroot/bin/ocamlrun"
mkdir -p "${PREFIX}/lib/findlib.conf.d"
cp esp32.conf "${PREFIX}/lib/findlib.conf.d"