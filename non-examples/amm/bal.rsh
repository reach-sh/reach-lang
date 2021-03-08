'reach 0.1';

const N = 3;

const mtArr = Array.replicate(N, 0);

const getReserves = (market) =>
  market.tokens.map(t => t.balance);

const swap = (amtIns, amtOuts, to, tokens, market) => {
  // Assert at least 1 token out
  assert(amtOuts.any(amt => amt > 0), "Insufficient amount out");

  // Reserves is how many of each token is in pool.
  const startingReserves = getReserves(market);

  // Assert amount outs are less than reserves of each token
  Array.zip(startingReserves, amtOuts).forEach(([reserve, amtOut]) =>
    assert(amtOut < reserve, "Insufficient liquidity"));

  // Transfer the given amount of tokens
  // XXX Feature - Pay in a specified token
  Array.zip(tokens, amtOuts)
    .forEach(([ tok, amtOut ]) =>
      transfer(amtOut).currency(tok).to(to));

  // Update market reserve balances.
  const updatedMarket = updateMarket(market, amtIns, amtOuts);

  // Get new reserve balances
  const tokenInfo = market.tokens;
  const weights   = market.params.weights;

  // Get actual balance (fees incorporated)
  const balances  = tokens.map(balanceOf);

  const resValue = tokenInfo
    .map(t => t.balance ** t.weight)
    .product();

  const balValue = balances
    .zip(weights)
    .map(([ balance, weight ]) => balance ** weight)
    .product();

  // Ensure the balances are at least as much as the reserves
  assert(balValue >= resValue, "V");

  // Update cumulative price if tracking
  return updatedMarket;
};

const getSpotPrice = (i, o, swapFee) => {
  const sp = (i.balance / i.weight) / (o.balance / o.weight);
  return sp * (1 / (1 - swapFee));
}

export const calcInGivenOut = ({ balanceIn, balanceOut, weightIn, weightOut, amtOut }) => {

  const v = balanceOut / (balanceOut / amtOut);
  const w = weightOut / weightIn;
  const u = v ** w;

  return balanceIn * (u - 1) * (1 / (1 - swapFee));
}

export const calcInGivenPrice = ({ balanceIn, balanceOut, weightIn, weightOut, desiredSP }) => {

  const curSP = getSpotPrice(
    { balance: balanceIn, weight: weightIn },
    { balance: balanceOut, weight: weightOut },
    swapFee);

  const u = weightOut / (weightOut + weightIn);
  const t = (desiredSP / curSP) ** u;

  return balanceIn * (t - 1);
}

const updateMarket = (market, amtIns, amtOuts) => ({
  params: market.params,
  tokens: Array.zip( market.tokens, Array.zip(amtIns, amtOuts) )
    .map(([ tp, [ amtIn, amtOut ] ]) =>
      ({
        balance: tp.balance + amtIn - amtOut,
        weight: tp.weight
      })),
});

const participants = [
  // XXX feature: better specification of participant entities
  Participant('Admin', {
      getParams : Fun([], {
        // Weight all token ratios should add up to
        totalWeight: UInt,
        // Weight for each token
        weights: Array(N, UInt),
        swapFee: UInt,
      }),
      shouldClosePool: Fun([], Null),
    }),
  Class('Provider', {
    allAssetDepositMaybe:
      Fun([], Tuple(Bool, Object({ amtIns: Array(UInt, N) }))),
    allAssetDepositDone:
      Fun([Bool, UInt], Null),

    allAssetWithdrawalMaybe:
      Fun([], Tuple(Bool, Object({ liquidity: UInt }))),
    allAssetWithdrawalDone:
      Fun([Bool, Array(N, UInt)], Null),

    singleAssetDepositMaybe:
      Fun([], Object({ inToken: UInt, amtIn: UInt })),
    singleAssetDepositDone:
      Fun([Bool, UInt], Null),

    singleAssetWithdrawalMaybe:
      Fun([], Object({ liquidity: UInt, outToken: UInt })),
    singleAssetWithdrawalDone:
      Fun([Bool, UInt], Null),
  }),
  Class('Trader', {}, {
    swapExactAmountInMaybe:
      Fun([], Tuple(Bool, Object({ minAmtOut: UInt, outToken: UInt, inToken: UInt, amtIn: UInt }))),
    swapExactAmountInDone:
      Fun([Bool, UInt], Null),
    swapExactAmountOutMaybe:
      Fun([], Tuple(Bool, Object({ amtOut: UInt, outToken: UInt, inToken: UInt, maxAmtIn: UInt }))),
    swapExactAmountOutDone:
      Fun([Bool, UInt], Null),
  }),
  Array(Token, N),
  MintedToken,
];

export const main =
  Reach.App(
    {},
    participants,
    (Admin, Provider, Trader, tokens, initialPool) => {

      Admin.only(() => {
        const params  = declassify(interact.getParams());
      });
      Admin.publish(params);

      const { weights } = params;

      const initialMarket = {
        params: params,
        tokens: weights.map(w => ({ balance: 0, weight: w })),
      };

      const [ alive, pool, market ] =
        parallelReduce([ true, initialPool, initialMarket ])
          .invariant(alive || pool.totalSupply() > 0)
          .while(true)
          .define(() => {
            const st = [ true, initialPool, initialMarket ];
            const wrap = (f, onlyIfAlive) => {
              const [ when, msg ] = declassify(f(st));
              return { when: (onlyIfAlive ? alive : true) && when, msg }
            }
          })
          // Admin functionality
          .case(Admin,
            (() => ({  when: alive && declassify(interact.shouldClosePool()) })),
            (() => [ false, pool, market ]),
           )
          .case(Trader,
            (() => wrap(interact.swapExactAmountInMaybe, true)),
            (({ amtIn, inToken }) => [ [ inToken, amtIn ] ]),
            (({ amtIn, inToken, outToken }) => {

              const tokenIn   = market.tokens[inToken];
              const balanceIn = tokenIn.balance;
              const weightIn  = tokenIn.weight;

              const v = weightIn / weightOut;
              const u = balanceIn / (balanceIn + amtIn * (1 - swapFee));
              const t = u ** v;

              const amtOut = balanceOut * (1 - t);

              require(amtOut >= minAmtOut, "Minimum amount out not met");

              const amtOuts = mtArr.set(outToken, amtOut);
              const amtIns  = mtArr.set(inToken, amtIn);

              const currentTrader = this;
              const updatedMarket = swap(amtIns, amtOuts, currentTrader, tokens, market);

              // Alert front end how many OutTokens trader received for swap
              Trader.only(() =>
                interact.swapExactAmountInDone(currentTrader == this, amtOut));

              return [ true, pool, updatedMarket ];

            }),
           )
          .case(Trader,
            (() => {
              const [ when, msg ] = declassify(interact.swapExactAmountOutMaybe(st));
              if (alive && when) {
                // Calculate what amtIn should be
                const tokenIn  = market.tokens[inToken];
                const tokenOut = market.tokens[outToken];

                const amtIn = calcInGivenOut({
                  balanceIn: tokenIn.balance,
                  balanceOut: tokenOut.balance,
                  weightIn: tokenIn.weight,
                  weightOut: tokenOut.weight,
                  amtOut: msg.amtOut
                });

                require(amtIn <= msg.maxAmtIn, "Maximum amtIn exceeded");

                return { when: true, msg : Object.set(msg, 'amtIn', amtIn) };
              } else {
                return { when: false, msg }; }
            }),
            (({ amtIn, inToken }) => [ [ inToken, amtIn ] ]),
            (({ amtIn, inToken, outToken, amtOut }) => {

              const currentTrader = this;
              const amtOuts = mtArr.set(outToken, amtOut);
              const amtIns  = mtArr.set(inToken, amtIn);
              const updatedMarket = swap(amtIns, amtOuts, currentTrader, tokens, market);

              // Alert front end how many InTokens trader received for swap
              Trader.only(() =>
                interact.swapExactAmountOutDone(currentTrader == this, amtIn));

              return [ true, pool, updatedMarket ];

            }),
           )
          // Provider functionality
          .case(Provider,
            (() => wrap(interact.allAssetDepositMaybe, true)),
            (({ amtIns }) => tokens.zip(amtIns) ),
            (({ amtIns }) => {

              const startingReserves = getReserves(market);
              const updatedMarket = updateMarket(market, amtIns, mtArr);
              const pSupply = pool.totalSupply();

              const minted = (pSupply == 0)
                ? sqrt(amtIns.product(), 10)
                : Array.zip(startingReserves, amtIn)
                  .map(([ sIn, amtIn ]) => pSupply * (amtIn / sIn - 1) - pSupply)
                  .average();

              const updatedPool = pool.mint(this, minted);

              // Alert front end of how many pool tokens were minted
              const currentProvider = this;
              Provider.only(() =>
                interact.allAssetDepositDone(currentProvider == this, minted));

              const currentProvider = this;
              Provider.only(() =>
                interact.allAssetDepositDone(currentProvider == this, minted));

              return [ true, updatedPool, updatedMarket ];

            }),
           )
           .case(Provider,
            (() => wrap(interact.allAssetWithdrawalMaybe, false)),
            (({ liquidity }) => {

              assert(liquidity <= pool.balanceOf(this),
                "Owner does not have requested liquidity to burn");

              const calcAllAssetWithdrawal = (bal) => {
                const u = (pSupply - liquidity) / pSupply;
                const t = 1 - u;
                return t * bal;
              };

              const amtOuts = tokens.map(compose(calcAllAssetWithdrawal, balanceOf));

              Array.zip(tokens, amtOuts)
                .forEach(([ tok, amtOut ]) =>
                  transfer(amtOut).currency(tok).to(this));

              const updatedMarket = updateMarket(market, mtArr, amtOuts);
              const updatedPool = pool.burn(this, liquidity);

              // Alert front end of how many tokens they withdrew
              const currentProvider = this;
              Provider.only(() =>
                interact.allAssetWithdrawalDone(currentProvider == this, amtOuts));

              const currentProvider = this;
              Provider.only(() =>
                interact.allAssetWithdrawalDone(currentProvider == this, amtOuts));

              return [ true, updatedPool, updatedMarket ];

            }),
           )
           .case(Provider,
            (() => wrap(interact.singleAssetDeposit, true)),
            (({ amtIn, inToken }) => [ [ inToken, amtIn ] ] ),
            (({ amtIn, inToken }) => {

              assert(pool.totalSupply() != 0,
                "Cannot make single asset deposit before pool is initialized.");

              const calcSingleAssetDeposit = ([{ balance, weight}, amt]) => {
                const w = amt - amt * (1 - weight) * swapFee;
                const v = 1 + (w / balance);
                const u = v ** weight;
                const t = u - 1;
                return pSupply * t;
              };

              const updatedMarket = updateMarket(market, amtIns, mtArr);
              const pSupply = pool.totalSupply();
              const minted = calcSingleAssetDeposit(market.tokens[inToken], amtIn);
              const updatedPool = pool.mint(this, minted);

              const currentProvider = this;
              Provider.only(() =>
                interact.singleAssetDepositDone(currentProvider == this, minted));

              return [ true, updatedPool, updatedMarket ];

            }),
           )
           .case(Provider,
            (() => wrap(interact.singleAssetWithdrawalMaybe, false)),
            (({ liquidity, outToken }) => {

              assert(liquidity <= pool.balanceOf(this),
                "Owner does not have requested liquidity to burn");

              const pSupply = pool.totalSupply();

              const calcSingleAssetWithdrawal = (weight, bal) => {
                const w = 1 - (liquidity / pSupply);
                const u = w ** (1 / weight);
                const t = 1 - u;
                return t * bal * (1 - (1 - weight) * swapFee);
              };

              const amtOut = calcSingleAssetWithdrawal(
                market.tokens[outToken].weight,
                balanceOf(tokens[outToken]));

              transfer(amtOut).currency(outToken).to(this);
              const updatedMarket = updateMarket(market, mtArr, amtOuts);
              const updatedPool = pool.burn(this, liquidity);

              const currentProvider = this;
              Provider.only(() =>
                interact.singleAssetWithdrawalDone(currentProvider == this, amtOut));

              return [ true, updatedPool, updatedMarket ];

            }),
           )

      commit();
      exit();
    });
