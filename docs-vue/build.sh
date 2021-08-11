#!/bin/sh
nvm use
npm run build
cp -f ../reach .vuepress/dist/
