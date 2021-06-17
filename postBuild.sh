#!/usr/bin/env bash

set -e

# snowpack keeps some source files in the dist folder, let's get rid of them
mv dist dist.old
mkdir dist
mv dist.old/index.html dist
mkdir -p dist/js
mv dist.old/js/*.js dist/js
mv dist.old/index.css dist
rm -r dist.old

# for some reason the css file does not have a hash in the name, let's fix it
cd dist
OLDNAME="index.css"
NEWNAME="$(md5sum ${OLDNAME} | awk '{ print $1 }').css"
mv ${OLDNAME} ${NEWNAME}
sed -i "s|$OLDNAME|$NEWNAME|" index.html
cd ..
