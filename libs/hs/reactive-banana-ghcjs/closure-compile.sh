closure-compiler --warning_level=QUIET\
                 --language_in=ECMASCRIPT5\
                 --compilation_level=ADVANCED_OPTIMIZATIONS\
                 --define='DEBUG=false'\
        dist/build/test/test.jsexe/all.js\
        > test.js

#cp jsbits/ReactiveBanana.js test.js
