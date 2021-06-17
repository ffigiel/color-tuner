#!/usr/bin/env bash

set -e


# Add content hash to js/css filenames
# Also append baseUrl in index.html, since snowpack/esbuild doesn't seem to handle it correclty

cd dist
OLDNAME="index.css"
NEWNAME="$(md5sum ${OLDNAME} | cut -c 1-6).css"
mv ${OLDNAME} ${NEWNAME}
sed -i "s|$OLDNAME|color-tuner/$NEWNAME|" index.html

OLDNAME="index.js"
NEWNAME="$(md5sum ${OLDNAME} | cut -c 1-6).js"
mv ${OLDNAME} ${NEWNAME}
sed -i "s|$OLDNAME|color-tuner/$NEWNAME|" index.html
cd ..
