# {#tut-nft} Minty Fresh NFT

Carla, tired of playing third string to Alice and Bob, has reinvented herself as an artist and wants to contribute to the growing creative crypto community.
Given her background in cryptography, she has heard of Reach and knows it's the safest, fastest, and smartest way to develop decentralized applications. 
Carla wants to use Reach to mint an NFT.
After reading Reach's [frontend](##frontend) and the [ARC-69](https://arc.algorand.foundation/ARCs/arc-0069) documentation, she understands almost everything she needs in order to mint an NFT on Algorand.

## {#tut-nft1} The Contract

Smart contract logic isn't required to mint on Algorand so this is the easiest backend Carla will ever write. 

``` rsh
load: index.rsh
md5: "e025d8afe22d00d3ce6968a1480e3139"
```

Carla only needs the backend in order to run the convenient devnet environment that Reach provides. 
With this file in place, Carla is able to execute `./reach run` to test that her frontend works in the devnet.

## {#tut-nft2} The Frontend

Now that the backend is complete, Carla is ready to build her frontend.

She begins by importing the Reach standard library and the test object. 

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 1-2
```

This is the standard beginning of Reach frontends and is not very interesting. 
Next, she creates test accounts for two participants: One who will mint the NFT(s) and another who will receive the non-network token(s).

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 4-6
```

To be efficient, Carla creates both participants on a single line using `{!js} newTestAccounts`.
This creates devnet accounts for testing her frontend's functionality similarly to `{!js} newTestAccount`, but can create multiple accounts in one statement.

Reach is interoperable across many chains. 
The next line limits gas fees when connecting to Ethereum. 

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 8-10
```

Line 8: Sets a gas limit in terms of Wei, Ethereum's atomic unit.
Lines 9-10: Applies the gas limit to the `minter` and `receiver` Participants. 

`{!js} setGasLimit` limits how much gas Ethereum will try to spend on any given transaction. 
Setting a gas limit will help Carla's minter avoid spending all of their ETH on gas while still successfully processing most transactions.

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 12-15
```

The next snippet features helpers for testing purposes. 
`{!js} getAddress` returns an account's address as a string. 
The address is printed as a convenience for testing purposes.

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 17-20
```

The account addresses are formatted per network specifications by `{!js} formatAddress` and then printed.
Carla will use the Receiver's formatted address when she makes the transfer of the NFT from the minter.

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 22-25
```

The next pattern formats the currency in a human-readable manner. 
`getBal` is a helper function to easily print an accounts balance, `bal`. 

## {#tut-nft3} There's Always an Option

Now that the preliminaries are complete, our next task in creating the NFT minter is to create an object to hold the ARC-69 options.
She formats the options to match [the protocol specs](https://arc.algorand.foundation/ARCs/arc-0069).

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 27-38
```

Line 27: A constant to hold a default name for the NFT
Line 28: A constant to hold the NFT's default symbol
Line 31: Indicates how many of the tokens to launch
Line 32: `au` refers to the asset's URL address. For our purposes, this is a dummy IPFS address
Line 33: `c` indicates if a clawback address is desired. `null` or an address should be used
Line 34: `f` is for a freeze address. An address may be supplied if freezing the address is desired
Line 35: If the asset is initially set to `defaultFrozen` then it cannot be transferred out of the minter's account
Line 36: `reserve` takes an address if a reserve address is desired
Line 37: A note in the form of a `{!js} Uint8Array` can be attached to the asset, if desired.

## {tut-nft4} Minty Fresh

Carla encapsulates the `{!js} launchToken` method in a function named `mintNFT`,
which is an asynchronous function that takes the minters address, name of the NFT, its symbol, and the ARC-69 options.
The options are supplied with default values that can be overridden when the function is called. 

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 40-45
```

Line 40: Establishes the `mintNFT` function with its parameters
Line 42: Launches the token using `{!js} launchToken` and the previously discussed arguments and stores the token in `theNFT`
Line 44: Returns the ID of the NFT

## {tut-nft5} Send it

Next, Carla writes the `transferNFT` function.

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 47-64
```

Line 47: Creates the asynchronous `transferNFT` function with parameters for minter, the receiver, the NFT ID, and the amount of the NFTs to transfer
Lines 49-50: Stores and shares the amount of NFTs in the minter's possession
Lines 52-57: If the compiler is connected to Algorand then the receiver opts-in to the token and we confirm that the opt-in was successful
Line 58: Makes the transfer of the NFT from the minter to the receiver, for the amount specified by `supply` of the NFT stored in `nftId`
Lines 61-62: Stores and prints the balance of the NFT in the `receiver`'s possession
Line 63: Is a test that confirms that the amount that was in the minter's wallet at the beginning of the function is the same as the receiver's at the end of the function. This test fails if the full balance was not transferred to the receiver.

In Algorand, an account, must first opt-in to an asset before it can hold any asset under its address. 
`{!js} tokenAccept` accepts the Algorand Standard Asset (what's often referred to as an NFT) and `{!js} tokenAccepted` ensures that the asset was successfully accepted.
If the asset wasn't successfully opted-in then `{!js} tokenAccepted` will exit with an error.

``` mjs
load: index.mjs
md5: "2b212db3d821bc8fbb728108f9fc63f5"
range: 66-70
```

The program is nearly complete. 
All that's left is to call the `mintNFT()` function and store the results in `nftId` so that `transferNFT()` may use the information as part of its arguments.
`transferNFT()` is called after `mintNFT()` has completed.
Finally, the new network token balance of the minter is shown to demonstrate how much they spent in gas fees.

Now that Carla's automated mint and transfer program is functional she'll create an interactive version to validate variations introduced by human input.