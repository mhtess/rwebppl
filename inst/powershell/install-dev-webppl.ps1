RWEBPPL=$args[0]
VERSION=$args[1]

cd $RWEBPPL/js
npm install webppl@$VERSION
mv node_modules/webppl webppl
rm -r node_modules
cd webppl
npm install
