#!/bin/bash
# shTEAL!
HERE=$(dirname "$0")
sig() {
  printf "%s" "$1" | hasher -a sha512_256 | cut -c 1-8
}

handle() {
  cat <<EOF
txna ApplicationArgs 0
byte 0x$(sig "$1")
==
bnz $(echo "$1" | cut -d '(' -f 1)
EOF
}

# Program
KI=$((0))
K_x=0x0$((KI++))
K_y=0x0$((KI++))
VI=$((0))
V_x=$((VI++))

cat <<EOF
#pragma version 6

// State
// Global: 0 byte, 2 uint

txn ApplicationID
bz ctor
$(handle "f(uint64)uint64")
err

ctor:
byte ${K_x}
int 0
app_global_put

byte ${K_y}
txna ApplicationArgs 1
btoi
app_global_put

b done

f:
byte 0x$(sig "return")
byte ${K_x}
dup
app_global_get
dup
store ${V_x}
int 1
+
app_global_put
load ${V_x}
byte ${K_y}
app_global_get
+
txna ApplicationArgs 1
btoi
+
itob
concat
log

b done

// Comment this in to show the warnings
//byte x

done:
int 1
EOF

