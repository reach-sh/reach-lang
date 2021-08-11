# DuoSwap

## Running Interactively via CLI

To run the program interactively, run:

    reach run -- i

If you want to run on the Ropsten testnet, run:

    reach run -- i testnet


## Contracts

Aside from the **Pool** contract that will exist for a pair of tokens.
There is an **Announcer** contract that will broadcast the addresses of pools.

## How To Run Interactively

You will be greeted with this when starting the interactive program:

```
Who are you?
1: DuoSwap Pool Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
```

You should follow the order of operations as described:

1. Run the `DuoSwap Announcer` which will launch the announcer contract.
It will then wait for you to input pool addresses via the CLI.

2. Run the `DuoSwap Token Funder` which will create the two tokens that
will be used for the pool contract. It will listen for addresses via the CLI
and fund them with both tokens.

3. Run the `DuoSwap Pool Admin`. First, it will print out
the address of the account. You will input this address into the `Token Funder`
CLI to fund the `Admin`. After, it will launch the pool contract and print
out the needed information to connect to the pool. This info will be used by
the `Providers` and `Traders`. It will then continuously ask whether you want
to close the pool.

4. Run the `DuoSwap Liquidity Provider`. With the printed address, fund the account with the `Token Funder` CLI. Then, it will listen for all the pool addresses that have been announced. When you find the pool you want to connect to, click Enter. Then, copy and paste the provided connection info. Finally, you can go about making an initial deposit, withdrawing, etc...

5. Run the `DuoSwap Trader`. With the printed address, fund the account with the `Token Funder` CLI. Then, it will listen for all the pool addresses that have been announced. When you find the pool you want to connect to, click Enter. Then, copy and paste the provided connection info. After, the `Provider` has made an initial deposit, make a trade.

Now that everyone has done something, you can close the pool via the `DuoSwap Admin`.
The pool will remain open until all the `Liquidity Providers` withdraw their minted
liquidity.

#

Note: The one thing less than ideal about running this program is whenever someone misses a race, i.e. another participant performs an action, the current CLI prompt is for the missed race. So if a Trader is asked to trade, and a LP makes a deposit in the mean time, the response to the trader's prompt is useless and has to be re-entered the _next_ time they are asked.

#

# Sample Interactive Session Log

## Announcer Manager

```
Who are you?
1: DuoSwap Pool Admin
  * Create a pool for a pair of tokens
2: DuoSwap Liquidity Provider
  * Receive liquidity tokens by depositing tokens into a pool
  * Withdraw liquidity from a pool
3: DuoSwap Trader
  * Trade one token for another in available pools
4: DuoSwap Announcer
  * Announces all the available pool addresses
5: DuoSwap Listener
  * Listens for all the available pool addresses
6: DuoSwap Token Funder
  * Create 2 tokens and fund any addresses you provide
4
Listening...
Announcer Contract Info: "0xD29749Ab3371D350C1Ea75634BD04b87e079dB93"
Enter new pool address:
0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480
Manager created pool: 0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480
Enter new pool address:
```

#

## Token Funder

```
Who are you?
1: DuoSwap Pool Admin
  * Create a pool for a pair of tokens
2: DuoSwap Liquidity Provider
  * Receive liquidity tokens by depositing tokens into a pool
  * Withdraw liquidity from a pool
3: DuoSwap Trader
  * Trade one token for another in available pools
4: DuoSwap Announcer
  * Announces all the available pool addresses
5: DuoSwap Listener
  * Listens for all the available pool addresses
6: DuoSwap Token Funder
  * Create 2 tokens and fund any addresses you provide
6
Creating first token...
Token symbol:
zmd
Token name:
zorkmid
Creating second token...
Token symbol:
gil
Token name:
gil
Launching token, zorkmid (zmd)
zmd: deploy
zmd: wait for deploy: 0x5d01dcec2155c3f2af3c1444c2b7c3b9587ce4b5770f6a0e1440232e25041e27
zmd: saw deploy: 7707
zmd: deployed: 0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5
Launching token, gil (gil)
gil: deploy
gil: wait for deploy: 0x3ec2bd8e5f1c1c4d85ac3d7a86eff7e4481731bbf948ff2e8d6c534f2878afe0
gil: saw deploy: 7709
gil: deployed: 0xB0776940303C47b66A7eE31b4084756AfEF684f9
Token Info: {"tokA":"0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5","tokB":"0xB0776940303C47b66A7eE31b4084756AfEF684f9"}
Ready To Mint 1000 zmd & 1000 gil
Address:
0xFe1daac0a2b73A3C565bcf0E1A9B6321610a316c
zmd: minting 1000000000000000000000 zmd for 0xFe1daac0a2b73A3C565bcf0E1A9B6321610a316c
gil: minting 1000000000000000000000 gil for 0xFe1daac0a2b73A3C565bcf0E1A9B6321610a316c
Ready To Mint 1000 zmd & 1000 gil
Address:
0x79C13496F70eF9fAE27D54726D4607b422e3D6Ec
zmd: minting 1000000000000000000000 zmd for 0x79C13496F70eF9fAE27D54726D4607b422e3D6Ec
gil: minting 1000000000000000000000 gil for 0x79C13496F70eF9fAE27D54726D4607b422e3D6Ec
Ready To Mint 1000 zmd & 1000 gil
Address:
0x2Dcf1178Cc8E79ae5AC274043e6CB286Ee15FF51
zmd: minting 1000000000000000000000 zmd for 0x2Dcf1178Cc8E79ae5AC274043e6CB286Ee15FF51
gil: minting 1000000000000000000000 gil for 0x2Dcf1178Cc8E79ae5AC274043e6CB286Ee15FF51
Ready To Mint 1000 zmd & 1000 gil
Address:
```

#

## DuoSwap Admin

```
Who are you?
1: DuoSwap Pool Admin
  * Create a pool for a pair of tokens
2: DuoSwap Liquidity Provider
  * Receive liquidity tokens by depositing tokens into a pool
  * Withdraw liquidity from a pool
3: DuoSwap Trader
  * Trade one token for another in available pools
4: DuoSwap Announcer
  * Announces all the available pool addresses
5: DuoSwap Listener
  * Listens for all the available pool addresses
6: DuoSwap Token Funder
  * Create 2 tokens and fund any addresses you provide
1
Fund: 0xFe1daac0a2b73A3C565bcf0E1A9B6321610a316c

Enter token info:
{"tokA":"0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5","tokB":"0xB0776940303C47b66A7eE31b4084756AfEF684f9"}
Enter Pool Address Into Announcer Manager: 0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480

Do you want to close the pool? (y/n)
n
Do you want to close the pool? (y/n)
n
Do you want to close the pool? (y/n)
n
Do you want to close the pool? (y/n)
n
Do you want to close the pool? (y/n)
n
Do you want to close the pool? (y/n)
y
```

#

## DuoSwap Liquidity Provider

```
Who are you?
1: DuoSwap Pool Admin
  * Create a pool for a pair of tokens
2: DuoSwap Liquidity Provider
  * Receive liquidity tokens by depositing tokens into a pool
  * Withdraw liquidity from a pool
3: DuoSwap Trader
  * Trade one token for another in available pools
4: DuoSwap Announcer
  * Announces all the available pool addresses
5: DuoSwap Listener
  * Listens for all the available pool addresses
6: DuoSwap Token Funder
  * Create 2 tokens and fund any addresses you provide
2
Fund: 0x79C13496F70eF9fAE27D54726D4607b422e3D6Ec

Paste Announcer Contract Info:
0xD29749Ab3371D350C1Ea75634BD04b87e079dB93
Searching for pools...
Click `Enter` when done searching for pools.
Listening...
 Pool ID: 0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480
   * 0 zmd
   * 0 gil
   * Info: {"poolAddr":"0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480","tokA":{"name":"zorkmid","symbol":"zmd","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5"},"tokB":{"name":"gil","symbol":"gil","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xB0776940303C47b66A7eE31b4084756AfEF684f9"}}

Enter connection info:
{"poolAddr":"0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480","tokA":{"name":"zorkmid","symbol":"zmd","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5"},"tokB":{"name":"gil","symbol":"gil","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xB0776940303C47b66A7eE31b4084756AfEF684f9"}}
Do you want to deposit? (y/n)
y
How much zmd do you want to deposit? (Bal: 1000 zmd & 1000 gil)
200
How much gil do you want to deposit? (Bal: 1000 zmd & 1000 gil)
10
 I received 62500000000000000000000000000000000009 pool tokens for my deposit of 200000000000000000000 zmd & 10000000000000000000 gil
Do you want to withdraw liquidity? (y/n)
n
Do you want to deposit? (y/n)
n
Do you want to withdraw liquidity? (y/n)
n
Do you want to deposit? (y/n)
n
Do you want to withdraw liquidity? (y/n)
n
Do you want to deposit? (y/n)
n
Do you want to withdraw liquidity? (y/n)
y
How much liquidity do you want to withdraw?
62500000000000000000000000000000000009
 I withdrew 200000002000000000000 zmd & 9999999900300000995 gil
Do you want to deposit? (y/n)
n
```

#

## DuoSwap Trader

```
Who are you?
1: DuoSwap Pool Admin
  * Create a pool for a pair of tokens
2: DuoSwap Liquidity Provider
  * Receive liquidity tokens by depositing tokens into a pool
  * Withdraw liquidity from a pool
3: DuoSwap Trader
  * Trade one token for another in available pools
4: DuoSwap Announcer
  * Announces all the available pool addresses
5: DuoSwap Listener
  * Listens for all the available pool addresses
6: DuoSwap Token Funder
  * Create 2 tokens and fund any addresses you provide
3
Fund: 0x2Dcf1178Cc8E79ae5AC274043e6CB286Ee15FF51

Paste Announcer Contract Info:
0xD29749Ab3371D350C1Ea75634BD04b87e079dB93
Searching for pools...
Click `Enter` when done searching for pools.
Listening...
 Pool ID: 0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480
   * 200000000000000000000 zmd
   * 10000000000000000000 gil
   * Info: {"poolAddr":"0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480","tokA":{"name":"zorkmid","symbol":"zmd","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5"},"tokB":{"name":"gil","symbol":"gil","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xB0776940303C47b66A7eE31b4084756AfEF684f9"}}

Enter connection info:
{"poolAddr":"0xb9bF0B40656fE84B4CaFd93328A4CCC59fD90480","tokA":{"name":"zorkmid","symbol":"zmd","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xEF352BB5AEA3b3a7fE54EF394B7BBd82759b18F5"},"tokB":{"name":"gil","symbol":"gil","url":"","metadata":"","supply":{"type":"BigNumber","hex":"0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"},"id":"0xB0776940303C47b66A7eE31b4084756AfEF684f9"}}
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
y
What token do you want to input?
zmd
gil
zmd
How much do you want to trade? (You have 1000 zmd)
0.000002
 I traded 2000000000000 zmd for 99699999005 gil
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
n
```

