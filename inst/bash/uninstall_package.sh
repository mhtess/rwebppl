#!/bin/bash

INST=$1
PKG=$2

if [ -d $INST ]
  then
    cd $INST
    npm uninstall $PKG
fi
