'reach 0.1';

const N = 2;

const TokenAmounts = Array(N, Tuple(Address, UInt));

const Swap = Object({
  amtIn: UInt,
  inToken: UInt,
  outToken: UInt,
});

const Deposit = Object({
  amtIns: Array(UInt, N),
  ratios: Array(UInt, N),
});

const Withdraw = Object({
  liquidity: UInt,
});

const State = Tuple(
  // Alive
  Bool,
  // Pool
  MintedToken,
  // Market
  Object({
    // Formula Valudation
    params: UInt,
    tokens: Array(N, Object({ balance: UInt })) }
  ))

const PARTICIPANTS = [
  // XXX: Feature - Better specification of entities
  Participant('Admin', {
    formulaValuation: UInt, // k
    shouldClosePool: Fun([State], Bool),
  }),
  Class('Provider', {
    withdrawMaybe: Fun([State], Tuple(Bool, Withdraw)),
    withdrawDone : Fun([Bool, TokenAmounts], Null),
    depositMaybe : Fun([State], Tuple(Bool, Deposit)),
    depositDone  : Fun([Bool, UInt], Null),
  }),
  Class('Trader', {
    tradeMaybe: Fun([State], Tuple(Bool, Swap)),
    tradeDone : Fun([Bool, TokenAmounts], Null),
  }),

  // XXX: Feature - Non-network token consumption
  // Token, Token       // Uniswap, specify 2
  Array(Token, N),

  // XXX: Feature - Token container (map-container-that-is-a-token)
  // JM: Because of Algorand, we'll need to have a built-in notion of a map-container-that-is-a-token and this would be an argument to Reach.DApp
  MintedToken,
];

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
  // Get new reserves & actual balances
  const reserves = getReserves(updatedMarket);
  const balances = tokens.map(balanceOf);

  // Ensure the balances are at least as much as the reserves
  assert(balances.product() >= reserves.product(), "K");

  // Update cumulative price if tracking
  return updatedMarket;
};

// Take into account .3% fee
const getAmountOut = (amtIn, reserveIn, reserveOut) => {
  // Calculate what amountIn was prior to fees
  const adjustedIn = amtIn * 997 / 1000;
  const reserveProduct = reserveOut * reserveIn;
  const adjustedReserveIn = reserveIn + adjustedIn;
  return reserveOut - (reserveProduct / adjustedReserveIn);
};


/**
 * Updates the balance of each market token by adding the
 * corresponding index of `amtIns`, and subtracting
 * the corresponding index of `amtOuts`.
 */
const updateMarket = (market, amtIns, amtOuts) => ({
  params: market.params,
  tokens: Array.zip( market.tokens, Array.zip(amtIns, amtOuts) )
    .map(([ tp, [ amtIn, amtOut ] ]) =>
      ({ balance: tp.balance + amtIn - amtOut })),
});

// Will ensure the deposit amounts and reserves satisfy the
// ratio of tokens, given in terms of the first token.
// E.g.
//            reserves = [2 ETH, 100 ALGO, 4 BAL]
//    amtIns (deposit) = [3 ETH, 150 ALGO, 6 BAL]
//               ratio = [1, 50, 2]
const satisfiesTokenRatio = (ratios, amts) => {
  const reference = amts[0];
  return Array.zip(ratios, amts)
    .all(([ ratio, amt]) =>
      reference * ratio == amt);
};

export const main =
  Reach.App(
    {},
    PARTICIPANTS,
    // tokens will only be used to grab balances, transfer, pay
    (Admin, Provider, Trader, tokens, initialPool) => {

      Admin.only(() => {
        const formulaValuation = declassify(interact.formulaValuation);
      });
      Admin.publish(formulaValuation);

      /*
        market : Object({
          params: ConstraintParams,
          tokens: Array(HowMany, TokenParams)
        })

        For UniSwap:
          ConstraintParams  = UInt // k
          TokenParams       = UInt // balance
      */
      const initialMarket = {
        params: formulaValuation,
        tokens: Array.replicate(N, { balance: 0 }),
      };

      const mtArr = Array.replicate(N, 0);

      // XXX Feature: Export some variables
      const [ alive, pool, market ] =
        parallelReduce([ true, initialPool, initialMarket ])
          .while(alive || pool.totalSupply() > 0)
          .invariant(true)
          // XXX Feature: `define` will allow definitions
          // to be in scope of all `case`s
          .define(() => {
            const st = [ alive, pool, market];
            const wrap = (f, onlyIfAlive) => {
              const [ when, msg ] = declassify(f(st));
              return { when: (onlyIfAlive ? alive : true) && when, msg };
            }
          })
          .case(
            Admin,
            (() => ({
              when: alive &&
                    declassify(interact.shouldClosePool(st))
            })),
            (() => [ false, pool, market ]))
          .case(
            Provider,
            (() => wrap(interact.withdrawMaybe, false)),
            (({ liquidity }) => {
              // Assert the Provider has the requested liquidity
              assert(liquidity <= pool.balanceOf(this),
                "Owner does not have requested liquidity to burn");

              // Balances have fees incorporated
              const balances = tokens.map(balanceOf);

              // The amount of each token in the reserve to return to Provider
              const amtOuts = balances.map(bal => liquidity * bal / pool.totalSupply());

              // Payout provider
              const payout = Array.zip(tokens, amtOuts);
              payout.forEach(([ tok, amtOut ]) =>
                transfer(amtOut).currency(tok).to(this));

              const updatedMarket = updateMarket(market, mtArr, amtOuts);

              /*
              XXX Feature: MintedToken.burn which behind the scenes does:
                  balanceOf[from] = balanceOf[from] - value;
                  totalSupply = totalSupply - value;
              */
              const updatedPool = pool.burn(this, liquidity);

              // Alert front end of their payout details
              const currentProvider = this;
              Provider.only(() =>
                interact.withdrawDone(currentProvider == this, payout));

              return [ true, updatedPool, updatedMarket ];
            }))
          .case(
            Provider,
            (() => wrap(interact.depositMaybe, true)),
            // XXX Feature: allow PAY_EXPR to make multiple payments in different currencies
            (({ amtIns }) => Array.zip(tokens, amtIns))
            (({ amtIns, ratios }) => {

              const startingReserves = getReserves(market);

              assert(satisfiesTokenRatio(ratios, startingReserves),
              "Starting reserves do not satisfy given token ratio.");

              // XXX To be implemented: Balancer allows a single asset deposit.
              // "Depositing a single asset A to a shared pool is equivalent to depositing all pool
              // assets proportionally and then selling more of asset A to get back all the other tokens deposited."
              assert(satisfiesTokenRatio(ratios, amtIns),
                "Must deposit pair tokens proportional to the current price");

              const updatedMarket = updateMarket(market, amtIns, mtArr);

              /*
                Mint liquidity tokens
                   If first deposit:
                     use geometric mean of inputs
                   Otherwise:
                     calculate the % of pool provided
              */
              const minted = (pool.totalSupply() == 0)
                ? sqrt(amtIns.product(), 10)
                : Array.zip(startingReserves, amtIns)
                  .map(([ sIn, amtIn ]) => (amtIn / sIn) * pool.totalSupply())
                  .average();

              /*
              XXX Feature: MintedToken.mint which behind the scenes does:
                  totalSupply = totalSupply + value;
                  balanceOf[to] = balanceOf[to] + value;
              */
              const updatedPool = pool.mint(this, minted);

              // Alert front end how many pool tokens were minted from deposit
              const currentProvider = this;
              Provider.only(() =>
                interact.tradeDone(currentProvider == this, minted));

              return [ true, updatedPool, updatedMarket ];
            }))
          .case(
            Trader,
            (() => wrap(interact.tradeMaybe, true)),
            // Amt in has fees incorporated into it
            (({ amtIn, inToken }) => [ [ amtIn, inToken ] ]),
            (({ amtIn, inToken, outToken }) => {
              // Calculate amount out
              const reserveIn  = market.tokens[inToken].balance;
              const reserveOut = market.tokens[outToken].balance;
              const amtOut  = getAmountOut(amtIn, reserveIn, reserveOut);

              // Get all outs and ins for tokens
              const amtOuts = mtArr.set(outToken, amtOut);
              const amtIns  = mtArr.set(inToken, amtIn);

              const to = this;
              const updatedMarket = swap(amtIns, amtOuts, to, tokens, market);

              // Alert front end of how much OutToken they received from their swap
              const currentTrader = this;
              Trader.only(() =>
                interact.tradeDone(currentTrader == this, amtOuts));

              return [ true, pool, updatedMarket ];
            }));

      commit();
      exit();
    }
  );
