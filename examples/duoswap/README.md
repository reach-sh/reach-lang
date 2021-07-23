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

2. Run the `DuoSwap Listener` which will listen for all the pool addresses
the `Manager` publishes.

3. Run the `DuoSwap Token Funder` which will create the two tokens that
will be used for the pool contract. It will listen for addresses via the CLI
and fund them with both tokens.

4. Run the `DuoSwap Admin`. First, it will print out
the address of the account. You will input this address into the `Token Funder`
CLI to fund the `Admin`. After, it will launch the pool contract and print
out the needed information to connect to the pool. This info will be used by
the `Providers` and `Traders`. It will then continuously ask whether you want
to close the pool.

5. Run the `DuoSwap Liquidity Provider`. With the printed address, fund the account with the `Token Funder` CLI. Then, provide the connection info that the
`Admin` printed out. Then, you can go about making an initial deposit, withdrawing, etc...

6. Run the `DuoSwap Trader`. With the printed address, fund the account with the `Token Funder` CLI. After, the `Provider` has made an initial deposit, make a trade.

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
Announcer Contract Info: "0x5AAF7232b4C2D43a69498bDe3f451Eab5c48f56D"
Enter new pool address:
0x3480eE966A3B16fb9185928D18b8a9C785A25E63
Manager created pool: 0x3480eE966A3B16fb9185928D18b8a9C785A25E63
Enter new pool address:
```

#

## Announcer Listener

```
Who are you?
1: DuoSwap Admin
2: DuoSwap Liquidity Provider
3: DuoSwap Trader
4: DuoSwap Announcer
5: DuoSwap Listener
6: DuoSwap Token Funder
5
Paste Announcer Contract Info:
0x5AAF7232b4C2D43a69498bDe3f451Eab5c48f56D
Listening...
Manager added pool: 0x3480eE966A3B16fb9185928D18b8a9C785A25E63
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
Launching token, zorkmid (ZMD)
ZMD: deploy
ZMD: wait for deploy: 0x7ee030e33f8c9dc15d4b7b097f02cc11645acbca11044d9426280ec72960a8a4
ZMD: saw deploy: 14042
ZMD: deployed: 0x7E074f2AcDAb2941f6A3588A19349E21c487F7CF
Launching token, gil (GIL)
GIL: deploy
GIL: wait for deploy: 0x19fc38a6b8f25d48fcaddc55c994235f671b9f775e1adc7bc1a45127ea47920d
GIL: saw deploy: 14044
GIL: deployed: 0xCD33184a0495f6bda6DCED04dCa459a6fC410124
Token Info: {"zmd":"0x7E074f2AcDAb2941f6A3588A19349E21c487F7CF","gil":"0xCD33184a0495f6bda6DCED04dCa459a6fC410124"}
Ready To Mint 1000 ZMD & 1000 GIL
Address:
0x03fE2fe0E95321a9D345bEc37cD6AF86EEA48f0d
ZMD: minting 1000000000000000000000 ZMD for 0x03fE2fe0E95321a9D345bEc37cD6AF86EEA48f0d
GIL: minting 1000000000000000000000 GIL for 0x03fE2fe0E95321a9D345bEc37cD6AF86EEA48f0d
Ready To Mint 1000 ZMD & 1000 GIL
Address:
0xf62CD572D9b5A171D87FD3aD625166F63E25B95f
ZMD: minting 1000000000000000000000 ZMD for 0xf62CD572D9b5A171D87FD3aD625166F63E25B95f
GIL: minting 1000000000000000000000 GIL for 0xf62CD572D9b5A171D87FD3aD625166F63E25B95f
Ready To Mint 1000 ZMD & 1000 GIL
Address:
0x61e415ccA90C0F48c81DEB7E4f5f822804c2b989
ZMD: minting 1000000000000000000000 ZMD for 0x61e415ccA90C0F48c81DEB7E4f5f822804c2b989
GIL: minting 1000000000000000000000 GIL for 0x61e415ccA90C0F48c81DEB7E4f5f822804c2b989
Ready To Mint 1000 ZMD & 1000 GIL
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
Fund: 0x03fE2fe0E95321a9D345bEc37cD6AF86EEA48f0d

Enter token info:
{"zmd":"0x7E074f2AcDAb2941f6A3588A19349E21c487F7CF","gil":"0xCD33184a0495f6bda6DCED04dCa459a6fC410124"}
Enter Pool Address Into Announcer Manager: 0x3480eE966A3B16fb9185928D18b8a9C785A25E63

Connection Info:  {"poolAddr":"0x3480eE966A3B16fb9185928D18b8a9C785A25E63","zmd":{"id":"0x7E074f2AcDAb2941f6A3588A19349E21c487F7CF","sym":"zmd","name":"zorkmid"},"gil":{"id":"0xCD33184a0495f6bda6DCED04dCa459a6fC410124","sym":"gil","name":"gil"}}
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
Fund: 0xf62CD572D9b5A171D87FD3aD625166F63E25B95f

Enter connection info:
{"poolAddr":"0x3480eE966A3B16fb9185928D18b8a9C785A25E63","zmd":{"id":"0x7E074f2AcDAb2941f6A3588A19349E21c487F7CF","sym":"zmd","name":"zorkmid"},"gil":{"id":"0xCD33184a0495f6bda6DCED04dCa459a6fC410124","sym":"gil","name":"gil"}}
Do you want to deposit? (y/n)
y
How much ZMD do you want to deposit? (Bal: 1000 zmd & 1000 gil)
200
How much GIL do you want to deposit? (Bal: 1000 zmd & 1000 gil)
100
 I received 625000000000000000000 pool tokens for my deposit of 200000000000000000000 ZMD & 100000000000000000000 GIL
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
625000000000000000000
 I withdrew 300000000000000000000 ZMD & 66733400066733400067 GIL
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
Fund: 0x61e415ccA90C0F48c81DEB7E4f5f822804c2b989

Enter connection info:
{"poolAddr":"0x3480eE966A3B16fb9185928D18b8a9C785A25E63","zmd":{"id":"0x7E074f2AcDAb2941f6A3588A19349E21c487F7CF","sym":"zmd","name":"zorkmid"},"gil":{"id":"0xCD33184a0495f6bda6DCED04dCa459a6fC410124","sym":"gil","name":"gil"}}
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
y
What token do you want to input?
ZMD
GIL
zmd
How much do you want to trade? (You have 1000 zmd)
100
 I traded 100000000000000000000 zmd for 33266599933266599933 gil
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
n
Do you want to trade? (y/n)
n
```

