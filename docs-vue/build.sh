#!/bin/sh
./patch.sh
npm run build
cp -f ../reach src/.vuepress/dist/
