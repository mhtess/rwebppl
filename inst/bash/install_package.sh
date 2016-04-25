#!/bin/bash

INST=$1
PKG=$2
RWEBPPL=$3

mkdir -p $INST
cd $INST
if [ ! -a ./package.json ]
  then
    cp "${RWEBPPL}/json/webppl-packages.json" ./package.json
fi
npm install $PKG
