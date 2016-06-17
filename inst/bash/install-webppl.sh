#!/bin/bash

RWEBPPL=$1

cd $RWEBPPL
cp json/rwebppl.json js/package.json

cd js
npm install

# Make this look as though it were symlinked, so we can use local paths
mv node_modules/webppl/ .
mv node_modules webppl/node_modules
