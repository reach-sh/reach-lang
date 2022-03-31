#!/bin/sh -xeu
CFX_CONFIG=/default.toml
export CFX_CONFIG

sh /daily-ping.sh &
./conflux --config "${CFX_CONFIG}" & #>/dev/null 2>&1 &
cd /faucet
exec npm start
