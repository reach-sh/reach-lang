. ../DEPS
docker run --rm \
    -v "$PWD:/mnt" \
    -u "$(id -ru):$(id -rg)" \
    -w /mnt \
    --entrypoint /usr/local/bin/solc \
    -ti \
    $SOLC_IMAGE \
    "${@}"