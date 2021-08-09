# DuoSwap

## Running Interactively via CLI

To run the program interactively, run:

    reach run -- i

NOTE: Providing any command line argument will cause the program to run
interactively.

## Contracts

Aside from the **Pool** contract that will exist for a pair of tokens.
There is an **Announcer** contract that will broadcast the addresses of pools.

## How To Run Interactively

You will be greeted with this when starting the interactive program:

```
Who are you?
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
```

You should follow the order of operations as described:

1. Run the `DuoSwap Announcer` which will launch the announcer contract.
It will then wait for you to input pool addresses via the CLI.

3. Run the `DuoSwap Token Funder` which will create the two tokens that
will be used for the pool contract. It will listen for addresses via the CLI
and fund them with both tokens.

4. Run the `DuoSwap Admin`. First, it will print out
the address of the account. You will input this address into the `Token Funder`
CLI to fund the `Admin`. After, it will launch the pool contract and print
out the needed information to connect to the pool. This info will be used by
the `Providers` and `Traders`. It will then continuously ask whether you want
to close the pool.

5. Run the `DuoSwap Liquidity Provider`. With the printed address, fund the account with the `Token Funder` CLI. Then, it will listen for all the pool addresses that have been announced. When you find the pool you want to connect to, click Enter. Then, copy and paste the provided connection info. Finally, you can go about making an initial deposit, withdrawing, etc...

6. Run the `DuoSwap Trader`. With the printed address, fund the account with the `Token Funder` CLI. Then, it will listen for all the pool addresses that have been announced. When you find the pool you want to connect to, click Enter. Then, copy and paste the provided connection info. After, the `Provider` has made an initial deposit, make a trade.

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
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
4
Listening...
Announcer Contract Info: "0x5cA3F5079a1d7EcD1e8831f5179C8baF9D9C8426"
Enter new pool address:
0xb259AE36142d2eb5b9B436821bb48247EE73Aac4
Manager created pool: 0xb259AE36142d2eb5b9B436821bb48247EE73Aac4
Enter new pool address:
```

#

## Token Funder

```
Who are you?
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
6
Creating first token...
Token symbol:
zmd
Token name:
zmd
Creating second token...
Token symbol:
gil
Token name:
gil
Launching token, zmd (zmd)
zmd: deploy
zmd: wait for deploy: 0x592788bf1c4b3eebe091b6f985b2643d918d60ad393390aa2e323eb703030c11
zmd: saw deploy: 2
zmd: deployed: 0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb
Launching token, gil (gil)
gil: deploy
gil: wait for deploy: 0x4bb2573bfecf71376e146bd81946d41fb9a487f28f2fb84cc548e8b5f785e700
gil: saw deploy: 4
gil: deployed: 0x57B38E81d1304709B03c9E3CaA224872867f6Af5
Token Info: {"tokA":{"id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb","sym":"zmd","name":"zmd"},"tokB":{"id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5","sym":"gil","name":"gil"}}
Ready To Mint 1000 zmd & 1000 gil
Address:
0xACBc25cAfB5078B4dF93A0f596b8C03c368f353b
zmd: minting 1000000000000000000000 zmd for 0xACBc25cAfB5078B4dF93A0f596b8C03c368f353b
gil: minting 1000000000000000000000 gil for 0xACBc25cAfB5078B4dF93A0f596b8C03c368f353b
Ready To Mint 1000 zmd & 1000 gil
Address:
0x192b84691C26e21849b041d66Fb9f010f8312363
zmd: minting 1000000000000000000000 zmd for 0x192b84691C26e21849b041d66Fb9f010f8312363
gil: minting 1000000000000000000000 gil for 0x192b84691C26e21849b041d66Fb9f010f8312363
Ready To Mint 1000 zmd & 1000 gil
Address:
0x311f1eAE006690112A55d0C59e2eC9e58664a6b6
zmd: minting 1000000000000000000000 zmd for 0x311f1eAE006690112A55d0C59e2eC9e58664a6b6
gil: minting 1000000000000000000000 gil for 0x311f1eAE006690112A55d0C59e2eC9e58664a6b6
Ready To Mint 1000 zmd & 1000 gil
Address:
```

#

## DuoSwap Admin

```
Who are you?
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
1
Fund: 0xACBc25cAfB5078B4dF93A0f596b8C03c368f353b

Enter token info:
{"tokA":{"id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb","sym":"zmd","name":"zmd"},"tokB":{"id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5","sym":"gil","name":"gil"}}
Enter Pool Address Into Announcer Manager: 0xb259AE36142d2eb5b9B436821bb48247EE73Aac4

Connection Info:  {"poolAddr":"0xb259AE36142d2eb5b9B436821bb48247EE73Aac4","tokA":{"id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb","sym":"zmd","name":"zmd"},"tokB":{"id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5","sym":"gil","name":"gil"}}
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
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
2
Fund: 0x192b84691C26e21849b041d66Fb9f010f8312363

Paste Announcer Contract Info:
0x5cA3F5079a1d7EcD1e8831f5179C8baF9D9C8426
Searching for pools...
Click `Enter` when done searching for pools.
Listening...
 Pool ID: 0xb259AE36142d2eb5b9B436821bb48247EE73Aac4
   * 0 zmd
   * 0 gil
   * Info: {"poolAddr":"0xb259AE36142d2eb5b9B436821bb48247EE73Aac4","tokA":{"sym":"zmd","name":"zmd","id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb"},"tokB":{"sym":"gil","name":"gil","id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5"}}

Enter connection info:
{"poolAddr":"0xb259AE36142d2eb5b9B436821bb48247EE73Aac4","tokA":{"sym":"zmd","name":"zmd","id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb"},"tokB":{"sym":"gil","name":"gil","id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5"}}
Do you want to deposit? (y/n)
y
How much zmd do you want to deposit? (Bal: 1000 zmd & 1000 gil)
200
How much gil do you want to deposit? (Bal: 1000 zmd & 1000 gil)
1
 I received 6250000000000000000 pool tokens for my deposit of 200000000000000000000 zmd & 1000000000000000000 gil
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
6250000000000000000
 I withdrew 300000000000000000000 zmd & 667334000667334001 gil
Do you want to deposit? (y/n)
n
```

#

## DuoSwap Trader

```
Who are you?
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
3
Fund: 0x311f1eAE006690112A55d0C59e2eC9e58664a6b6

Paste Announcer Contract Info:
0x5cA3F5079a1d7EcD1e8831f5179C8baF9D9C8426
Searching for pools...
Click `Enter` when done searching for pools.
Listening...
 Pool ID: 0xb259AE36142d2eb5b9B436821bb48247EE73Aac4
   * 200000000000000000000 zmd
   * 1000000000000000000 gil
   * Info: {"poolAddr":"0xb259AE36142d2eb5b9B436821bb48247EE73Aac4","tokA":{"sym":"zmd","name":"zmd","id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb"},"tokB":{"sym":"gil","name":"gil","id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5"}}

Enter connection info:
{"poolAddr":"0xb259AE36142d2eb5b9B436821bb48247EE73Aac4","tokA":{"sym":"zmd","name":"zmd","id":"0x555d86fac9cc1630322bD2Bd890A994Bf005a6eb"},"tokB":{"sym":"gil","name":"gil","id":"0x57B38E81d1304709B03c9E3CaA224872867f6Af5"}}
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
y
What token do you want to input?
zmd
gil
zmd
How much do you want to trade? (You have 1000 zmd)
100
 I traded 100000000000000000000 zmd for 332665999332665999 gil
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
n
```

