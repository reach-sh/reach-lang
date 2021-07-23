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
