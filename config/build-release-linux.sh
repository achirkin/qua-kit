#!/bin/bash
set -e

## The aim of this script is to build haskell projects using stack

# First, find the project directory
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
PROJDIR=${DIR%/config}

pushd "${PROJDIR}"

# NOW DO THE MAIN SCRIPT IN THE ROOT PROJECT FOLDER

echo "Creating release folders in the project root (${PROJDIR}/release/linux)"
mkdir -p "${PROJDIR}/release/linux/services"

echo "Building qua-server..."
stack build --install-ghc --stack-yaml apps/hs/qua-server/stack.yaml --flag qua-server:-postgresql
cp -rf "${PROJDIR}/apps/hs/qua-server/static" "${PROJDIR}/release/linux/services/static"
cp -f "${PROJDIR}/apps/hs/qua-server/$(stack path --stack-yaml apps/hs/qua-server/stack.yaml --dist-dir)/build/qua-server/qua-server" "${PROJDIR}/release/linux/services/qua-server"

echo "Building helen..."
stack build --install-ghc --stack-yaml apps/hs/helen/stack.yaml
cp -f "${PROJDIR}/apps/hs/helen/$(stack path --stack-yaml apps/hs/helen/stack.yaml --dist-dir)/build/helen/helen" "${PROJDIR}/release/linux/helen"

echo "Building siren..."
stack build --install-ghc --stack-yaml services/siren/stack.yaml
cp -f "${PROJDIR}/services/siren/$(stack path --stack-yaml services/siren/stack.yaml --dist-dir)/build/siren/siren" "${PROJDIR}/release/linux/services/siren"
cp -f "${PROJDIR}/services/siren/sirenDBcreate.sql" "${PROJDIR}/release/linux/services/sirenDBcreate.sql"
cp -f "${PROJDIR}/services/siren/sirenDBconfigure.sql" "${PROJDIR}/release/linux/services/sirenDBconfigure.sql"


echo "Building example services..."
stack build --install-ghc --stack-yaml services/examples-hs/stack.yaml
cp -f "${PROJDIR}/services/examples-hs/$(stack path --stack-yaml services/examples-hs/stack.yaml --dist-dir)/build/hs-example-service/hs-example-service" "${PROJDIR}/release/linux/services/hs-example-service"


echo "Finished successfully!"
cp -f "${PROJDIR}/config/example-helen-config.yaml" "${PROJDIR}/release/linux/helen-config.yaml"
cp -f "${PROJDIR}/config/README-linux.md" "${PROJDIR}/release/linux/README.md"
cp -f "${PROJDIR}/config/setup-sirendb-linux.sh" "${PROJDIR}/release/linux/services/setup-sirendb.sh"
if [ -x "$(command -v unzip)" ]; then
  mkdir -p "${PROJDIR}/release/linux/geojson-examples"
  unzip -qo "${PROJDIR}/config/geojson-examples.zip" -d "${PROJDIR}/release/linux/geojson-examples"
else
  cp -f "${PROJDIR}/config/geojson-examples.zip" "${PROJDIR}/release/linux/geojson-examples.zip"
fi
popd



