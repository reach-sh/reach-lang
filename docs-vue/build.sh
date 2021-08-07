#!/bin/sh
. ~/.nvm/nvm.sh
nvm use
npm run build
cp -f ../reach .vuepress/dist/
