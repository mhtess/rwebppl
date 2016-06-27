#!/bin/bash

RWEBPPL=$1

cd $RWEBPPL/js

# Make this look as though it were symlinked, so we can use local paths
mv node_modules/webppl/ .
mv node_modules webppl/node_modules
