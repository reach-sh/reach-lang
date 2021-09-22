#!/bin/sh
./patch.sh
yarn build
cp -f ../reach src/.vuepress/dist/
