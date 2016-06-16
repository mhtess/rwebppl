#!/bin/bash

WEBPPL=$1
RWEBPPL=$2

cd $RWEBPPL
npm link webppl

#mkdir -p $WEBPPL
#cd $WEBPPL
#cp "${RWEBPPL}/json/rwebppl.json" ./package.json
#cp "${RWEBPPL}/js/rwebppl" .
#npm install --save webppl
#cd webppl
#npm update webppl