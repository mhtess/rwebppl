#!/bin/bash

RWEBPPL=$1

cd $RWEBPPL/js

mv node_modules/webppl/ .
mv node_modules webppl/node_modules
