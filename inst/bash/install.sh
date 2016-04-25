#!/bin/bash

WEBPPL=$1
RWEBPPL=$2

mkdir -p $WEBPPL
cd $WEBPPL
cp "${RWEBPPL}/json/rwebppl.json" ./package.json
cp "${RWEBPPL}/js/rwebppl" .
npm install --save webppl
