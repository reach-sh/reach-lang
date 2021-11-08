SHELL=/bin/bash

.PHONY: convert
convert:
	set -o pipefail ; racket -t make.rkt 2>&1 | tee log
