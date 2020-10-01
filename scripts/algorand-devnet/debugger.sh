#!/bin/sh -xe
rm -fr debug
racket server.rkt 2>&1 | tee debug.log
