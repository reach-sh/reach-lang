= Algorand Connector Notes

The main pending feature for Algorand is linear state and non-network token
support.
This document describes the plan for how these will work.

= Linear state

Reach's linear state is linear in the number of participants (i.e. we support
mappings from addresses to values.)
This maps directly to Algorand's notion of local storage associated with an
application (`app_local_put`/`app_local_get`): basically, the Algorand protocol
bakes into the ledger a mapping from applications to values.
We will leverage this by making it so that Reach programs with local space will
manage this region and joining the application will entail opting in to allow
it access to this storage.
Like all things Algorand, there is a limit on how much space can be associated
in this region, so we will detect at compile-time when an application uses too
much state.

As a special case, Reach will support a kind of linear state that is a token
managed by the Reach the program.
This has advantages on Ethereum, because we can mandate an ERC-20 interface
in parallel to the rest of the application.
On Algorand, this will compile to the deployer of the application creating a
new ASA.
Although on Ethereum, we could supplement the ERC-20 interface handlers with
"notifications" into the Reach program, we do not know if this is possible in
Algorand and presume it is not.

= Non-network token support

As mentioned before, linear state subsumes the creation and management of
tokens.
In this case, we mean the consumption of tokens.
On Ethereum, this compiles to the ERC-20 interface, while on Algorand it
compiles to the ASA interface.
The main distinction is that on Ethereum, the ERC-20 interface is a foreign
call, while on Algorand, the ASA interface is a different kind of transaction.
This means that we will adapt the TEAL compiler to statically assign
transaction group slots to transactions dealing with particular tokens.
The main downside of this that we see is that in a program like an AMM, a
trader may need to submit 2 x N transactions for N tokens if we cannot
statically label withdrawing vs depositing transactions.
