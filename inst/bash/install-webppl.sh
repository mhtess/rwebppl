#!/bin/bash

RWEBPPL=$1

cd $RWEBPPL
cp json/rwebppl.json js/package.json

cd js
npm install
