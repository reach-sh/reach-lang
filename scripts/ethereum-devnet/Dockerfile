FROM ethereum/client-go:v1.9.2

RUN apk add curl

ADD run.sh /run.sh
ADD genesis.json /genesis.json

# ENV PREFUNDER_PRIVATE_KEY '0x36fd2f4431f5c6d4db1eaff38c58baedb7ea76b83ad894d16d5e86f47c5cff5e'
ENV PREFUNDER_ACCOUNT '0xC4E63766429c6aDA912d52b889a32953319749b8'
# ENV PREFUNDER_AMOUNT '999999999999999999999999999999'
ENV PREFUNDER_ENC_KEY '{"address":"c4e63766429c6ada912d52b889a32953319749b8","id":"b695046b-6366-46a6-9ef9-8850d88faeb3","version":3,"Crypto":{"cipher":"aes-128-ctr","cipherparams":{"iv":"d273a1d6979ea7de13b8d9938b918802"},"ciphertext":"5dc537e8e843aedef8cc90a29084973c5d9e2af1686e8bb94ae35d1840873530","kdf":"scrypt","kdfparams":{"salt":"0cf0ec28c79d266b87c229c0dc5560d4a15407642606e67f83aa602f27c31bbb","n":131072,"dklen":32,"p":1,"r":8},"mac":"925449ab399b6aac30c25d94eda0dcebb792738f662c96cc3178f0c5d086337c"}}'

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' http://localhost:8545
