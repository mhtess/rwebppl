#!/bin/bash

RWEBPPL=$1

cd $RWEBPPL
cp json/webppl-packages.json js/package.json

cd js
npm install webppl

# Make this look as though it were symlinked, so we can use local paths
mv node_modules/webppl/ .
mv node_modules webppl/node_modules
