#!/bin/bash

# we want that all elm-stuff stay in src/main/elm
# whatever the path from which this script is called
ELM_DIR="$( cd "$( dirname "$0" )" && pwd )"
cd $ELM_DIR
# node details
elm-make sources/PackageScanNode.elm --output=generated/package-scan-node.js --yes
