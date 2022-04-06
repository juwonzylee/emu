#!/bin/bash -x
./_build/default/bin/main.exe | dot -Gdpi=300 -Tpdf > output/example1.pdf
