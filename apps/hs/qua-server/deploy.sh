#/bin/sh

cabal clean
cabal configure --enable-optimization=2 --disable-profiling --disable-library-profiling
cabal build
cp dist/build/qua-server/qua-server .
tar czpf qua-server.keter qua-server web config/keter.yaml
rm qua-server
scp qua-server.keter achirkin@mooc.ia.arch.ethz.ch:/opt/keter/incoming
