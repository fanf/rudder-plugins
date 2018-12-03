#!/bin/bash

./src/main/elm/build-app.sh
#cp src/main/elm/generated/package-scan-config.js target/classes/toserve/packagescan/
cp src/main/elm/generated/package-scan-node.js target/classes/toserve/packagescan/
