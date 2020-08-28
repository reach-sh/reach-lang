#!/bin/sh

make build || exit 1

docker-compose up -d alice bob || exit 1

rm -f Alice.in Alice.out Bob.in Bob.out
mkfifo Alice.in Alice.out Bob.in Bob.out || exit 1

docker attach tut-7_alice_1 < Alice.in > Alice.out &
APID=$!
docker attach tut-7_bob_1 < Bob.in > Bob.out &
BPID=$!

exec 3> Alice.in
exec 4< Alice.out
exec 5> Bob.in
exec 6< Bob.out
unlink Alice.in
unlink Alice.out
unlink Bob.in
unlink Bob.out

get_Alice() {
    read -r <&4
    echo Alice: "$REPLY"
}
to_Alice() {
    echo Alice: "$@"
    echo "$@" >&3
}
get_Bob() {
    read -r <&6
    echo Bob: "$REPLY"
}
to_Bob() {
    echo Bob: "$@"
    echo "$@" >&5
}

get_Alice
get_Alice
get_Alice
get_Alice
get_Alice
to_Alice y
get_Alice
get_Alice
to_Alice y
get_Alice
to_Alice y
get_Alice
INFO=$(echo "$REPLY" | awk -F= '{print $2}')
get_Alice
get_Alice
to_Alice 10

get_Bob
get_Bob
get_Bob
get_Bob
get_Bob
to_Bob n
get_Bob
get_Bob
to_Bob y
get_Bob
to_Bob n
get_Bob
to_Bob "$INFO"
get_Bob
get_Bob
to_Bob y

get_Alice
to_Alice r
get_Alice
get_Bob
to_Bob r
get_Bob

get_Alice
to_Alice s
get_Alice
get_Bob
to_Bob s
get_Bob

get_Alice
to_Alice r
get_Alice
get_Bob
to_Bob s
get_Bob

get_Alice
get_Bob
get_Alice
get_Bob

kill "${APID}" "${BPID}"
kill -9 "${APID}" "${BPID}"
