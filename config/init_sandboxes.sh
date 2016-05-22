#!/bin/bash
set -e
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
CSOURCES=`cabal sandbox list-sources`
if [ `echo "${CSOURCES}" | grep -c "${PROJDIR}/libs/hs/ghcjs-hs-interop"` -eq "0" ]; then
    cabal sandbox add-source "${PROJDIR}/libs/hs/ghcjs-hs-interop"
fi
if [ `echo "${CSOURCES}" | grep -c "${PROJDIR}/libs/hs/ghcjs-webgl"` -eq "0" ]; then
    cabal sandbox add-source "${PROJDIR}/libs/hs/ghcjs-webgl"
fi
if [ `echo "${CSOURCES}" | grep -c "${PROJDIR}/libs/hs/fastvec"` -eq "0" ]; then
    cabal sandbox add-source "${PROJDIR}/libs/hs/fastvec"
fi
cabal install --dependencies-only
cabal configure
popd

# setup qua-server
pushd "${PROJDIR}/apps/hs/qua-server"
echo "compiler: ghc" > cabal.config
cabal sandbox init --sandbox "${PROJDIR}/.ghc-sandbox"
CSOURCES=`cabal sandbox list-sources`
if [ `echo "${CSOURCES}" | grep -c "${PROJDIR}/libs/hs/ltiv1p1"` -eq "0" ]; then
    cabal sandbox add-source "${PROJDIR}/libs/hs/ltiv1p1"
fi
cabal install --dependencies-only
cabal configure
popd
