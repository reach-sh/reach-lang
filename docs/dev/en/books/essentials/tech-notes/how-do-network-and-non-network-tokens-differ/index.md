---
author: Jay McCarthy
hasOtp: true
menuItem: mi-docs
publishedDate: 2021-10-02T14:00:00
---

# How do network and non-network tokens differ?

Reach assumes that network tokens and non-network tokens behave identically on consensus networks, but this is not the case in practice.

## How do network tokens behave?

An account on a consensus network can hold network tokens and as long as the network is available, it may send these tokens to other accounts or receive tokens from other accounts without preapproval from recipients and without the interference of third-parties not involved in a particular transfer. Each network may prescribe fees on transfers or other similar constraints on transfers (such as a minimum balance holding to send funds) but may not restrict the reception of funds or arbitrarily hold or reclaim network tokens. All consensus networks that Reach supports obey these properties.

## How do non-network tokens violate this?

In each of the networks supported by Reach, non-network tokens may violate these expectations.

On Ethereum, and Ethereum-like networks, where non-network tokens are just particular patterns of smart contracts, the concrete behavior of abstract operations like "Transfer 10 Zorkmids from John to Paul" may have arbitrary semantics. For example, a smart contract insist that Paul pre-approve the reception of Zorkmids; a smart contract could allow all transfers to be universally halted, like an old fashion bank closure; a smart contract could simply take John’s Zorkmids away because the administrator of the token decides to; and so on. Smart contracts have power to implement arbitrary semantics and there is no guarantee that a smart contract that supplies a function with the name transfer and the signature function transfer(address _to, uint256 _value) public returns (bool success) has any particular behavior. Furthermore, you can send network tokens to a contract as you create it, but you cannot send non-network tokens, because sending non-network tokens (using ERC-20) requires knowing a contract’s address, which you cannot know until after you create it (unless you use a particular low-level creation operation).

On Algorand, non-network tokens are built-in to the network, so they have a stable and predictable semantics, but that semantics is different than network tokens. For example, non-network token reception must be pre-approved so John cannot transfer to Paul unless Paul has predetermined he is willing to accept Zorkmids. Furthermore, non-network token creation supports options which have further differences: it may be possible to "freeze" all transfers, so that no one can make any transfers; and, it may be possible to "clawback" balances, so that John’s Zorkmids can be removed from his account without his intervention.

On each network, it is possible to minimize these differences—by disabling these options and obeying a standard semantics—but that behavior is not universal among all tokens.

Non-network tokens minted by Reach always disable these options and behave as closely as possible to network tokens.

## Why does this matter?

These issue matter because developers and users of their applications need to understand that when they interact with a non-network token, they are interacting with a third party that can potentially control their application’s behavior.

For example, suppose George and Ringo decide to play poker on a consensus network and bet Zorkmids, rather than network tokens. If the manager of Zorkmids, Zorkmanager, freezes them, then the game must stop. If the game requires that hands be provided in a timely fashion, then George could bribe Zorkmanager to freeze them every time it is Ringo’s turn, forcing him to forfeit a round.

Suppose at the end of the game, there is a pot of 200 Zorkmids with 5 meant for George and 195 meant for Ringo. If Zorkmanager takes 1 Zorkmid from the pot via "clawback", then only one of the parties can be paid in full. What’s worse, an application may be programmed to either transfer everything or nothing, so in this scenario if George extracts first, then Ringo will not be able to extract anything. Furthermore, suppose the application is programmed to clear the pot atomically, disbursing to each player in one single step; in this scenario, if recipients are required to pre-authorize holding a token, then George can revoke that permission to spite Ringo and prevent him from getting his allocation.

In summary, the semantics of non-network tokens is non-intuitive given the power given to their creators.

## What does Reach do about this?
Reach uses a verification engine to model the semantics of Reach programs to predict and reason about their behavior. In particular, it tries to prove two theorems:

* Honesty: Honest participants will not submit transactions that will be rejected.
* Progress: If honest participants submit transactions, the program will finish.

If a program contains an operations, such as "Transfer 10 tokens to John", then there are certain pre-conditions that must be true for this operation to succeed, such as "The contract holds at least 10 tokens". Reach will guarantee that every pre-condition in the program is entailed by the earlier parts of the program. When pre-conditions depend on user input, it will ensure that honest participants check that input before submitting the transaction.

Non-network tokens, because they are arbitrary code on some networks and depend on transient state controlled by third-party on other networks, have no semantics, and therefore, have no predictable pre-conditions. This means that it is impossible to predict whether an operation will succeed or fail simply by knowing it (for example) follows the ERC-20 specification or is an Algorand Standard Asset.

In our design of Reach, we had three choices of what to do this.

First, we could represent non-network token operations as free terms with no semantics, and thus unpredictable behavior. We did not do this, because users expect them have a particular behavior (and most actually do!) and expect that Reach’s token linearity verification will apply to non-network tokens as well.

Second, we could explicitly represent the details of the power each network gives to non-network token creators and include their state space in the analysis of Reach programs. We did not do this, because it is impossible to write programs that are generic in the non-network tokens they use with this token. In other words, a Reach program that implemented a poker game could be a game that used some token, it would have to be a game that used Zorkmids and you’d have to write (or verify) another program to have game that used Gil, and so on. Given that most tokens actually behave properly, this would be unnecessarily painful for productive programming.

Third, we can assume that non-network tokens behave the same as network tokens and document the differences and educate developers and users about the consequences of this. Clearly, this is what we did.

## What can I do about it?
First, if you use non-network tokens, you need to understand that you are trusting the token issuer as much as you are trusting the consensus network itself. This means that you need to audit its code, or configuration, and decide if you can place trust in its manager.

Second, if your token requires pre-authorization of receipt, and if this pre-authorization can be revoked, you need to remove atomic simultaneous transfers of non-networks from your program and replace them with phases where each party can receive their tokens individually, so that one party cannot maliciously opt-out to prevent the other party from receiving their funds.

Finally, you can advocate, perhaps with your money and support, that consensus networks pursue giving non-network tokens feature parity with network tokens so that there will be a consensus network that can faithfully implement the token semantics users expect.