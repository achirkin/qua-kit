#/bin/sh

cabal clean
cabal configure --enable-optimization=2 --disable-profiling --disable-library-profiling
cabal build
cp dist/build/qua-server/qua-server .
tar czpf qua-server.keter qua-server web static config/keter.yaml
rm qua-server
scp qua-server.keter qua-kit.ethz.ch:/opt/keter/incoming
