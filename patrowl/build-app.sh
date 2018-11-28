#!/bin/bash

./src/main/elm/build-app.sh
cp src/main/elm/generated/patrowl-config.js target/classes/toserve/patrowl/
cp src/main/elm/generated/patrowl-node.js target/classes/toserve/patrowl/
