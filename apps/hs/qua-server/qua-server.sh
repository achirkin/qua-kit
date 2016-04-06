#!/bin/bash
set -e
## Check if qua-view is ready, and then run qua-server

# First, find the project directory
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
PROJDIR=${DIR%/apps/hs/qua-server}
QVDIR=${PROJDIR}/apps/hs/qua-view

cd $DIR # I am sorry, but you must be in this folder

# check sandboxes
if [ -f cabal.sandbox.config ] && [ -f "${QVDIR}/cabal.sandbox.config" ]
then
    echo "Using existing sandboxes."
else
    pushd "$PROJDIR/config"
    ./init_sandboxes.sh
    popd
fi

# check if there a compiled js available
if [ -f "${QVDIR}/web/qua-view.js" ]
then
    echo "Using existing qua-view build."
else
    pushd $QVDIR
    cabal configure --enable-optimization=2 --disable-profiling --disable-library-profiling
    cabal build
    ./closure-compile.sh
    popd
fi

# qua-view resources folder
if [ -d "web" ]; then
    rm -rf web
fi
mkdir web
cp $QVDIR/web/qua-view.js web/
cp $QVDIR/web/qua-view.html web/
cp $QVDIR/web/qua-view.css web/
cp $QVDIR/web/numeric.min.js web/

# finally run the project using cabal
env PORT=8070 cabal run qua-server
