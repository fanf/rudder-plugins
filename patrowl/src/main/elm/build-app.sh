#!/bin/bash

# we want that all elm-stuff stay in src/main/elm
# whatever the path from which this script is called
ELM_DIR="$( cd "$( dirname "$0" )" && pwd )"
cd $ELM_DIR
# main plugin page
elm-make sources/PatrowlConfig.elm --output=generated/patrowl-config.js --yes
# node details
elm-make sources/PatrowlNode.elm --output=generated/patrowl-node.js --yes
