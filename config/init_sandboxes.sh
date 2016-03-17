#!/bin/bash

## The aim of this script is to set up cabal sandboxes for ghc and ghcjs apps

# First, find the project directory
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
PROJDIR=${DIR%/config}

# setup qua-view
pushd "${PROJDIR}/apps/hs/qua-view"
echo "compiler: ghcjs" > cabal.config
cabal sandbox init --sandbox "${PROJDIR}/.ghcjs-sandbox"
cabal sandbox add-source "${PROJDIR}/libs/hs/ghcjs-base-alt"
cabal sandbox add-source "${PROJDIR}/libs/hs/ghcjs-webgl"
cabal sandbox add-source "${PROJDIR}/libs/hs/fastvec"
cabal install --dependencies-only
cabal configure
popd

# setup qua-server
pushd "${PROJDIR}/apps/hs/qua-server"
echo "compiler: ghc" > cabal.config
cabal sandbox init --sandbox "${PROJDIR}/.ghc-sandbox"
cabal install --dependencies-only
cabal configure
popd
