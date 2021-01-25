'reach 0.1';

const N = 2;

/**
 * ===================================================
 * Value function
 * ===================================================
 *
 * An invariant that implies a spot price
 * at each point st. no matter what exchanges are carried
 * out, the share of value of each token in the pool remains
 * constant:
 *
 *  V = Π t => balanceT ** WeightT
 *
 * weightT : fraction representing share of pool
 *           ( total weights add up to 1 )
 */

/**
 * ===================================================
 * Spot Price
 * ===================================================
 *
 * Each pair of tokens in a pool has a spot price. It is
 * defined solely in terms of the balance and weights of
 * each token in the pair. The spot price of any two tokens,
 * SpotPriceIO, or SPio, is the ratio of token balances,
 * normalized by their weights:
 *
 * let i = balanceI / weightI
 * let o = balanceO / weightO
 * in
 * SPio = i / o
 *
 * With constant weights, spot prices will only change
 * based on token balances.
 *
 */

/**
 * ===================================================
 * Effective Price
 * ===================================================
 *
 * SPio is the theoretical price for trades which incur
 * no slippage. The effective price for a trade depends
 * on the amount traded, which always causes a price
 * change. We can define EPio as:
 *
 *  EPio = amtIn / amtOut
 *
 * EP tends to SP, when trade amounts tend to 0
 *
 */

/**
 * ===================================================
 * Trading: Out-Given-In
 * ===================================================
 *
 * When a user sends tokens `i` to get tokens `o`,
 * all other token balances remain the same. Since
 * the value function after the trade should remain
 * the same as before, we can calculate the amtOut
 * from:
 *
 * Π (k /= i, o) =>
 *   let others = (balanceK ** weightK)
 *   let out    = (balanceOut - amtOut) ** weightOut
 *   let in     = (balanceIn + amtIn) ** weightIn
 *   let inv    = Π k => balanceK ** weightK
 *   in
 *   others * out * in = inv
 *
 * which simplifies to:
 *
 * let v = balanceIn / (balanceIn + amtIn)
 * let w = weightIn / weightOut
 * let u = v ** w
 * let t = 1 - u
 * in
 * amtOut = balanceOut * t
 *
 * This function performs a swap for a Trader.
 */
const TradeOutGivenInArgs = Object({
  outToken : UInt,
  inToken  : UInt,
  amtIn    : UInt,
});


/**
 * ===================================================
 * Trading: In-Given-Out
 * ===================================================
 *
 * It's also useful for traders to know how much to send of
 * tokenIn to get a desired amount of tokenOut. We can calc
 * that by the same formula above, but solving for amtOut:
 *
 * let v = balanceOut / (balanceOut / amtOut)
 * let w = weightOut / weightIn
 * let u = v ** w
 * let t = u - 1
 * in
 * amtIn = balanceIn * t
 *
 * This function just returns info to Trader.
 */
const CalcInGivenOutArgs = Object({
  inToken  : UInt,
  outToken : UInt,
  amtOut   : UInt,
});


/**
 * ===================================================
 * Trading: In-Given-Price
 * ===================================================
 *
 * Traders who want to take advantage of arbitrage would like
 * to know how many tokenIn they will have to send to change
 * spot price of SPio to a desired SP'io. The formula to
 * calc it is:
 *
 * let v = SP'io / SPio
 * let w = weightOut / (weightOut + weightIn)
 * let u = v ** w
 * let t = u - 1
 * in
 * amtIn = balanceIn * t
 *
 * This function just returns info to Trader.
 */
const CalcInGivenPriceArgs = Object({
  inToken   : UInt,
  outToken  : UInt,
  desiredSP : UInt,
});

/**
 * ===================================================
 * Liquidity Deposit: All-Asset Deposit
 * ===================================================
 *
 * An "all-asset" deposit must have all the assets in the
 * right proportions. To receive pIssued pool tokens,
 * given an existing total supply of pSupply, one must
 * deposit dK tokens to the pool.
 *
 * let u = (pSupply + pIssued) / pSupply
 * let t = u - 1
 * in
 * dK = t * balanceK
 *
 * balanceK: balance of token k before deposit
 *
 * Solved for pIssued:
 *
 * let u = (dK / balanceK) + 1
 * let t = pSupply * u
 * in
 * pIssued = t - pSupply
 *
 */
const AllAssetDepositArgs = Object({
  amtIns : Array(UInt, N),
});

/**
 * ===================================================
 * Liquidity Withdrawal: All-Asset Withdraw
 * ===================================================
 *
 * Provider redeems their pool tokens in return for a
 * proportional share of each asset in pool. To calc
 * the amount of each token to withdraw from the pool:
 *
 * let u = (pSupply - pRedeemed) / pSupply
 * let t = 1 - u
 * in
 * aK = t * balanceK
 *
 * balanceK: balance of token k before withdrawal
 */
const AllAssetWithdrawalArgs = Object({
  liquidity: UInt,
});

/**
 * ===================================================
 * Liquidity Deposit: Single-Asset Deposit
 * ===================================================
 *
 * Providers may depsoit single asset to pool, if pool
 * contains that asset. Depositing a single asset A, is
 * equivalent to depositing all pool assets proportionally
 * and then selling more of asset A to get back all the
 * other tokens deposited. This way a provider would end up
 * spending only asset A, since the amounts of other tokens
 * deposited would be returned through the trades.
 *
 * let u = V' / V
 * let t = u - 1
 * in
 * pIssued = pSupply * t
 *
 * V' : value after deposit
 * V  : value before deposit
 *
 * which simplifies to:
 *
 * let v = amtT / balanceT
 * let w = 1 + v
 * let u = w ** weightT
 * let t = u - 1
 * in
 * pIssued  = pSupplied * t
 *
 * t : token used in single deposit
 *
 */
const singleAssetDepositArgs = Object({
  amtInToken : UInt,
  amtIn      : UInt,
});

/**
 * ===================================================
 * Liquidity Withdrawal : Single-Asset Withdrawal
 * ===================================================
 *
 * let w = 1 - (pReedemed / pSupply)
 * let u = w ** (1 / weightT)
 * let t = 1 - u
 * in
 * amtT = balanceT * t
 *
 * balanceT: balance of token before withdrawal
 *
 */
const singleAssetWithdrawalArgs = Object({
  liquidity : UInt,
  outToken  : UInt,
});

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
  // XXX: Feature - Pay in a specified token
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
  Participant('Admin', {
    getParams : Fun([], {
      formulaValuation: UInt,
      totalWeight: UInt,        // Weight all token ratios should add up to
      weights: Array(N, UInt),  // Weight for each token
      swapFee: UInt,
    }),
    shouldClosePool: Fun([], Bool),
  }),
  Class('Provider', {
    allAssetDeposit       : Fun([AllAssetDepositArgs], Null),
    allAssetWithdrawal    : Fun([AllAssetWithdrawalArgs], Null),
    singleAssetDeposit    : Fun([singleAssetDepositArgs], Null),
    singleAssetWithdrawal : Fun([singleAssetWithdrawalArgs], Null),
  }),
  Class('Trader', {
    tradeOutGivenIn : Fun([TradeOutGivenInArgs], Null),
    calcInGivenOut  : Fun([CalcInGivenOutArgs], UInt),
    calcInGivenPrice: Fun([CalcInGivenPriceArgs], UInt),
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
        parallel_reduce([ true, initialPool, initialMarket ])
          .invariant(alive || pool.totalSupply() > 0)
          .while(true)
          // Admin functionality
          .case(Admin,
            (() => ({
              exposeAs: "closePool",
              when: declassify(interact.shouldClosePool())
            })),
            (() => [ false, pool, market ]),
           )
          // Trader Functionality
          // XXX : This is just to give client info as
          // opposed to contributing to the parallel reduce
          // accumulator. How should this be handled? It
          // needs to access market.
          .case(Trader,
            (() => ({
              exposeAs: "calcInGivenPrice",
              msg: declassify(interact.calcInGivePrice())
            })),
            (({ inToken, outToken, desiredSP }) => {
              const tokenOut = market.tokens[outToken];
              const tokenIn  = market.tokens[inToken];

              const weightOut = market.weights[outToken];
              const weightIn  = market.weights[inToken];

              const balanceIn = tokenIn.balance;

              const curSP = getSpotPrice(tokenIn, tokenOut, swapFee);

              const u = weightOut / (weightOut + weightIn);
              const t = (desiredSP / curSP) ** u;

              return balanceIn * (t - 1);
            }),
           )
          // XXX Just gives client info as well
          .case(Trader,
            (() => ({
              exposeAs: "calcInGivenOut",
              msg: declassify(interact.calcInGivenOut())
            })),
            (({ inToken, outToken, amtOut }) => {
              const tokenOut = market.tokens[outToken];
              const tokenIn  = market.tokens[inToken];

              const weightOut = market.weights[outToken];
              const weightIn  = market.weights[inToken];

              const balanceOut = tokenOut.balance;
              const balanceIn  = tokenIn.balance;

              const v = balanceOut / (balanceOut / amtOut);
              const w = weightOut / weightIn;
              const u = v ** w;

              // XXX : This is just to give client info as
              // opposed to contributing to the parallel reduce
              // accumulator. How should this be handled? It
              // needs to access market.
              return balanceIn * (u - 1) * (1 / (1 - swapFee));
            }),
           )
          .case(Trader,
            (() => ({
              exposeAs: "tradeOutGivenIn",
              msg: declassify(interact.tradeOutGivenIn()),
            })),
            (({ amtIn, inToken }) => [ inToken, amtIn ]),
            (({ amtIn, inToken, outToken }) => {

              const tokenIn   = market.tokens[inToken];
              const balanceIn = tokenIn.balance;
              const weightIn  = tokenIn.weight;

              const v = weightIn / weightOut;
              const u = balanceIn / (balanceIn + amtIn * (1 - swapFee));
              const t = u ** v;

              const amtOut = balanceOut * (1 - t);

              const amtOuts = mtArr.set(outToken, amtOut);
              const amtIns  = mtArr.set(inToken, amtIn);

              const to = this;
              const updatedMarket = swap(amtIns, amtOuts, to, tokens, market);

              return [ true, pool, updatedMarket ];

            }),
           )
          // Provider functionality
          .case(Provider,
            (() => ({
              exposeAs: "allAssetDeposit",
              msg: declassify(interact.allAssetDeposit()),
              when: alive,
            })),
            (({ amtIns }) => tokens.zip(amtIns) ),
            (({ amtIns }) => {

              const startingReserves = getReserves(market);
              const updatedMarket = updateMarket(market, amtIns, mtArr);
              const pSupply = pool.totalSupply();

              const minted = (pSupply == 0)
                ? sqrt(amtIns.product())
                : Array.zip(startingReserves, amtIn)
                  .map(([ sIn, amtIn ]) => pSupply * (amtIn / sIn - 1) - pSupply)
                  .average();

              const updatedPool = pool.mint(this, minted);

              return [ true, updatedPool, updatedMarket ];

            }),
           )
           .case(Provider,
            (() => ({
              exposeAs: "allAssetWithdrawal",
              msg: declassify(interact.allAssetWithdrawal()),
              when: alive,
            })),
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

              return [ true, updatedPool, updatedMarket ];

            }),
           )
           .case(Provider,
            (() => ({
              exposeAs: "singleAssetDeposit",
              msg: declassify(interact.singleAssetDeposit()),
              when: alive,
            })),
            (({ amtIn, amtInToken }) => [ [ amtInToken, amtIn ] ] ),
            (({ amtIn, amtInToken }) => {

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
              const minted = calcSingleAssetDeposit(market.tokens[amtInToken], amtIn);
              const updatedPool = pool.mint(this, minted);

              return [ true, updatedPool, updatedMarket ];

            }),
           )
           .case(Provider,
            (() => ({
              exposeAs: "singleAssetWithdrawal",
              msg: declassify(interact.singleAssetWithdrawal()),
              when: alive,
            })),
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

              return [ true, updatedPool, updatedMarket ];

            }),
           )

      commit();
      exit();
    });
