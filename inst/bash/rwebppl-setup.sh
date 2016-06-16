#!/bin/bash

WEBPPL=$1
RWEBPPL=$2

cd $WEBPPL
mkdir rwebppl
cd rwebppl
cp "${RWEBPPL}/json/rwebppl.json" ./package.json
cp "${RWEBPPL}/js/rwebppl" .
