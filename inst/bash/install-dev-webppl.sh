#!/bin/bash

RWEBPPL=$1
COMMIT=$2

cd $RWEBPPL/js
git clone https://github.com/probmods/webppl.git
cd webppl
git checkout $COMMIT
npm install