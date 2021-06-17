#!/bin/sh -e

sed -i.bak 's/const DEADLINE = 10;/const DEADLINE = 100;/' index.rsh
make build || exit 1
mv index.rsh.bak index.rsh

REACH_CONNECTOR="$(echo "$REACH_CONNECTOR_MODE" | cut -f 1 -d '-')"
DOCKER_COMPOSE_YML_DEFAULT="docker-compose.${REACH_CONNECTOR}.yml"
DOCKER_COMPOSE_YML="${1:-"${DOCKER_COMPOSE_YML_DEFAULT}"}"

docker-compose -f "$DOCKER_COMPOSE_YML" up -d alice bob || exit 1

rm -f Alice.in Alice.out Bob.in Bob.out
mkfifo Alice.in Alice.out Bob.in Bob.out || exit 1

docker attach tut-8_alice_1 < Alice.in > Alice.out &
APID=$!
docker attach tut-8_bob_1 < Bob.in > Bob.out &
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
    echo Waiting for Alice...
    read -r REPLY <&4
    echo Alice:-: "$REPLY"
}
to_Alice() {
    echo Alice:+: "$@"
    echo "$@" >&3
}
get_Bob() {
    echo Waiting for Bob...
    read -r REPLY <&6
    echo "  Bob:-:" "$REPLY"
}
to_Bob() {
    echo "  Bob:+:" "$@"
    echo "$@" >&5
}

while [ "x$REPLY" != "xAre you Alice?" ] ; do
  get_Alice
done
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

while [ "x$REPLY" != "xAre you Alice?" ] ; do
  get_Bob
done
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
