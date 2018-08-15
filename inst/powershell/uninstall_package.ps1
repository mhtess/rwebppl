$INST=$args[0]
$PKG=$args[1]

$CHKDIR=Test-Path $INST
If ( $CHKDIR ) {
    cd $INST
    npm uninstall $PKG
}
