$INST=$args[0]
$PKG=$args[1]
$RWEBPPL=$args[2]

mkdir -p $INST
cd $INST
$CHKFILE=Test-Path ./package.json
If ( -Not $CHKFILE ) {
    cp $RWEBPPL/json/webppl-packages.json ./package.json
}
npm install $PKG
