#!/bin/sh

rm test.js
./closure-compile.sh
scp test.html w3ia@ia.arch.ethz.ch:~/htdocs/
scp test.js w3ia@ia.arch.ethz.ch:~/htdocs/

