= Automatic Market Maker in Reach

This is sorted under `non-examples`, because it relies on features
yet-to-be-finalized in Reach.

This is an implementation of an automatic market maker (AMM), like UniSwap, Balancer,
and so on.
Our implementation is compile-time parameterized over policy parameters, like
the identity of tokens, token balancing formulae, and so on.
Thus, it abstracts away details that other AMM make concrete, like how UniSwap
v2 is for two tokens with the constant product formula rule.

--

It produces one token: the pool token.

It consumes N tokens: the market tokens.

It has three roles:
- The administrator
- The liquidity providers
- The traders

The administrator sets the parameters of the pool, like its initial formula
valuation and whether it is active.

Liquidity providers can deposit or withdraw market tokens into the pool provided they satisfy the formula;
they withdraw or deposit pool tokens when they do this.

Traders may swap market tokens in a way that satisfies (and potentially
updates) the formula.
They do not interact with pool tokens.

--

The structure is something like:

```reach
Admin.publish(params);

const [ alive, pool, market ] =
  parallelReduce( [ true, mempty, mempty ] )
  .while( alive )
  .case(Admin,
    /* maybe decide to close the pool */)
  .case(Provider,
    /* maybe decide to deposit / withdraw */)
  .case(Trader,
    /* maybe submit a trade */);

exit();
```

The `pool` value would be a token container managed by the application.
This could be implemented as a mapping from addresses to balances, but it may
be better for this kind of functionality to be first-class in Reach.
In particular, this would allow transfers to be handled behind-the-scenes of
the DApp.

The `market` value would be an array of structures holding balance information
about each of the market tokens.
This would not use linear state.

--

This program will rely on a few unreleased features:

- Foreign to Reach interface specifications --- This is so that Providers &
  Traders may be instantiated by on-chain principals, rather than only by
  Reach-generated off-chain backends.

- Non-network token consumption --- These are the market tokens.

- Bespoke token production (a.k.a. special case of linear state) --- This is
  the pool token.

--

Links and resources:
- https://uniswap.org/docs/v2/protocol-overview/how-uniswap-works
- https://github.com/Uniswap/uniswap-v2-core/blob/master/contracts/UniswapV2Pair.sol
- https://github.com/balancer-labs/balancer-core/blob/master/contracts/BPool.sol
- https://balancer.finance/whitepaper/
- https://docs.balancer.finance
