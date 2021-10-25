#!/bin/sh
exec ganache-cli -h 0.0.0.0 -p 7545 --accounts 1 --defaultBalanceEther 1000000000
