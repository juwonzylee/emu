#!/bin/bash -x
# outputting example json file into output folder
# harmtrace parse --json --grammar=jazz --chords="C:maj D:min;1 G:7;2 C:maj;1" 
harmtrace parse --json --grammar=jazz --chords="$*" | tail -n 1 \
| ./_build/default/bin/main.exe 
