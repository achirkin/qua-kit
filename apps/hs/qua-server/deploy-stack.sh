#/bin/sh

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

pushd $DIR 

# no check for sandboxes!
# check if there a compiled js available
if [ -f "${QVDIR}/web/qua-view.js" ]
then
    echo "Using existing qua-view build."
else
    echo "Need to have qua-view.js!"
    return
fi

# qua-view resources folder
if [ -d "web" ]; then
    rm -rf web
fi
mkdir web
cp $QVDIR/web/qua-view.js web/

pushd $PROJDIR
# build qua-server using stack
stack clean --stack-yaml=qua-server.yaml
stack build --ghc-options=-O2 --copy-bins --stack-yaml=qua-server.yaml
mv `stack path --local-bin  --stack-yaml=qua-server.yaml`/qua-server $DIR/
popd


# pack archive
tar czpf qua-server.keter qua-server web static config/keter.yaml
rm qua-server

# deploy
scp qua-server.keter qua-kit.ethz.ch:/opt/keter/incoming

popd
