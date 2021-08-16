


# {#guide-changelog} Reach's Changelog

Below is a list of changes to Reach.
Versions and changes-within-versions are listed in reverse-chronological order: newest things first.

## 0.1.3: 2021/07 - present

Version 0.1.3 is the current Reach release candidate version.
+ 2021/07/31: Added `newTestAccounts`, `waitUntilSecs`, and `getNetworkSecs` to JavaScript standard library.
+ 2021/07/31: Updated `onProgress` type in JavaScript standard library.
+ 2021/07/31: Added `relativeTime`, `absoluteTime`, `relativeSecs`, `absoluteSecs`, `baseWaitTime`, `baseWaitSecs`, and `lastConsensusSecs` to Reach, with support in `wait` and `.timeout`.
+ 2021/07/22: `numberToFixedPoint()` and `numberToInt()` added.
+ 2021/07/21: Renamed Ethereum devnet Docker image to `devnet-eth`.
+ 2021/07/21: Renamed connector modes to use naming convention `$NET-devnet`, rather than exposing implementation.
+ 2021/07/21: Ethereum contract bytecode verification changed to directly compare deployment data
+ 2021/07/20: `Array.slice()` added.
+ 2021/07/19: `Token.destroyed()` added.
+ 2021/07/15: Ethereum contract info (i.e. `ctc.getInfo()`) reduced to address only.
+ 2021/07/14: Algorand contract info (i.e. `ctc.getInfo()`) reduced to application id only.
+ 2021/07/14: Minted tokens must be destroyed by end of application.
+ 2021/07/14: Token minting support added to Algorand.
+ 2021/07/14: Token URL metadata increased to 96 bytes.
+ 2021/07/14: Algorand `digest` switched to SHA256 (to save compute cost).
+ 2021/07/14: Algorand connector updated to AVM 0.9 (TEAL version 4)
+ 2021/07/14: Algorand devnet version updated to 2.7.1, plus `DevMode` patch
+ 2021/07/14: Algorand devnet image renamed to `devnet-algo`
+ 2021/07/14: version tagged


## 0.1.2: 2020/09 - 2021/07

Version 0.1.2 is the current Reach release version.

It is the last version that supports Algorand using TEAL3; if you deployed a contract on Algorand using Reach version 0.1.2, you will need to continue accessing it via the 0.1.2 version of the Reach standard library.

+ 2021/07/09: `.define` component added to `parallelReduce`
+ 2021/07/08: [Error code referenec](##ref-error-codes) created
+ 2021/06/20: Token minting introduced, with implementation on ETH.
+ ... many interesting things ...
+ 2020/09/01: version tagged


## 0.1.1: 2019/09 - 2020/09

Version 0.1.1 was used prior to our documented release process.

## 0.1.0: 2019/09 - 2020/09

Version 0.1.0 was used prior to our documented release process.
