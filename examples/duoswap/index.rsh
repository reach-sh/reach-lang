'reach 0.1';

export const NUM_OF_TOKENS = 2;

const MkArray = (ty) => Array(ty, NUM_OF_TOKENS);

const avg = (a, b) => (a + b) / 2;

const getAmtOut = (amtIn, reserveIn, reserveOut) => {
  const amtInWithFee = amtIn * 997;
  const num = amtInWithFee * reserveOut;
  const den = (reserveIn * 1000) + amtInWithFee;
  return num / den;
}

export const main = Reach.App(() => {
  // Types
  const Market = Object({
    k: UInt
  });

  const State = Tuple(Bool, Market);

  const Withdraw = Object({
    liquidity: UInt
  });

  const Deposit = Object({
    amtA: UInt,
    amtB: UInt
  });

  const Trade = Object({
    amtA: UInt,
    amtB: UInt,
    amtInTok: Token,
  });

  // Participants
  const ProviderInterface = {
    ...hasConsoleLogger,
    withdrawMaybe: Fun([State], Object({
      when: Bool,
      msg : Withdraw,
    })),
    withdrawDone: Fun([Bool, MkArray(UInt)], Null),
    depositMaybe: Fun([State], Object({
      when: Bool,
      msg: Deposit,
    })),
    depositDone: Fun([Bool, UInt, UInt, UInt], Null),
  };

  const Provider = ParticipantClass('Provider', ProviderInterface);

  const Admin = Participant('Admin', {
    tokA: Token,
    tokB: Token,
    shouldClosePool: Fun([State], Object({
      when: Bool,
      msg : Null,
    })),
  });

  const Trader = ParticipantClass('Trader', {
    ...hasConsoleLogger,
    logMarket: Fun(true, Null),
    tradeMaybe: Fun([State], Object({
      when: Bool,
      msg : Trade,
    })),
    tradeDone: Fun([Bool, Tuple(UInt, Token, UInt, Token)], Null),
  });

  const Tokens = View('Tokens', {
    aTok : Token,
    bTok : Token,
    // aBal : UInt,
    // bBal : UInt,
  });

  deploy();

  const s18 = (x) => x / 1000 / 1000 / 1000 / 1000 / 1000 / 1000;

  // Admin sets up initial pool by making first deposit
  Admin.only(() => {
    const tokA = declassify(interact.tokA);
    const tokB = declassify(interact.tokB);
    assume(UInt.max > 0);
    assume(tokA != tokB);
  });

  Admin.publish(tokA, tokB);

  require(UInt.max > 0);
  require(tokA != tokB);

  Tokens.aTok.set(tokA);
  Tokens.bTok.set(tokB);

  assert(balance(tokA) == 0);
  assert(balance(tokB) == 0);

  const initialMarket = {
    k: balance(tokA) * balance(tokB)
  };

  const totalSupply = UInt.max;
  const pool = new Token({ supply: totalSupply });

  // Calculates how many LP tokens to mint
  const mint = (amtIn, bal, poolMinted) => {
    return (poolMinted * amtIn) / ((bal == 0) ? 1 : bal);
  };

  const newK = (amtIn, ainTok, amtOut, amtOutTok) =>
    (balance(amtOutTok) - amtOut) * (balance(ainTok) + amtIn);

  assert(balance(pool) == totalSupply);

  const [ alive, market, poolMinted ] =
    parallelReduce([ true, initialMarket, 0 ])
      .define(() => {
        const st = [ alive, market ];
        // Tokens.aBal.set(balance(tokA));
        // Tokens.bBal.set(balance(tokB));
        const wrap = (f, onlyIfAlive) => {
          const { when, msg } = declassify(f(st));
          return { when: (declassify(onlyIfAlive) ? alive : true) && when, msg };
        }
        const constantProduct = () => {
          const x = balance(tokA);
          const y = balance(tokB);
          return x * y == market.k;
        };
      })
      .invariant(
        balance() == 0 &&
        pool.supply() == totalSupply &&
        totalSupply == balance(pool) + poolMinted &&
        constantProduct())
      .while(alive || poolMinted > 0)
      .paySpec([ pool, tokA, tokB ])
      .case(Admin,
        (() => wrap(interact.shouldClosePool, true)),
        (() => { return [ false, market, poolMinted ]; })
      )
      .case(Provider,
        (() => {
          if (poolMinted > 0) {
            const { when, msg } = declassify(interact.withdrawMaybe(st));
            assume(poolMinted > 0);
            assume(msg.liquidity <= poolMinted, "liquidity <= poolMinted");
            assume(balance(tokA) > 0 && balance(tokB) > 0, "bal(tokA) > 0 && bal(tokB) > 0");
            return { when, msg };
          } else {
            return { when: false, msg: { liquidity: 0 }};
          }
        }),
        (({ liquidity }) => [ 0, [ liquidity, pool ], [ 0, tokA ], [ 0, tokB ] ]),
        (({ liquidity }) => {
          require(poolMinted > 0);
          require(liquidity <= poolMinted, "liquidity <= poolMinted");
          require(balance(tokA) > 0 && balance(tokB) > 0, "bal(tokA) > 0 && bal(tokB) > 0");

          // Balances have fees incorporated
          const balances = array(UInt, [ balance(tokA), balance(tokB) ]);

          // Amount of each token in reserve to return to Provider
          const amtOuts = balances.map(bal => liquidity * bal / poolMinted);

          // Payout provider
          const currentProvider = this;
          transfer(amtOuts[0], tokA).to(currentProvider);
          transfer(amtOuts[1], tokB).to(currentProvider);

          // Update market
          const marketP = { k: balance(tokA) * balance(tokB) }

          // Inform frontend of their payout
          Provider.only(() => {
            interact.withdrawDone(currentProvider == this, amtOuts) });

          return [ true, marketP, poolMinted - liquidity ];
        })
      )
      .case(Provider,
        (() => {
          const { when, msg } = wrap(interact.depositMaybe, true);

          // Ensure minted amount is less than pool balance
          if (when) {
            const { amtA, amtB } = msg;
            const minted =
              (poolMinted == 0)
                ? s18(sqrt(amtA * amtB, 4))
                : avg( mint(amtA, balance(tokA), poolMinted), mint(amtB, balance(tokB), poolMinted) );
            assume(minted > 0, "minted > 0");
            assume(minted < balance(pool), "assume minted < balance(pool)");
            return { when, msg: { amtA, amtB, minted } };
          } else {
            return { when: false, msg: { amtA: 0, amtB: 0, minted: 0 } };
          }
        }),
        (({ amtA, amtB }) => [0, [ 0, pool ], [ amtA, tokA ], [ amtB, tokB] ]),
        (({ amtA, amtB, minted }) => {
          require(minted > 0, "minted > 0");
          require(minted < balance(pool), "require minted < balance(pool)");

          // Pay liquidity provider their pool tokens
          const currentProvider = this;
          transfer(minted, pool).to(currentProvider);

          // Update market
          const marketP = { k: balance(tokA) * balance(tokB) }

          // Inform frontend of their deposit
          Provider.only(() => {
            interact.depositDone(currentProvider == this, amtA, amtB, minted) });

          return [ true, marketP, poolMinted + minted ];
        })
      )
      .case(Trader,
        (() => {
          const { when, msg } = wrap(interact.tradeMaybe, true);
          const { amtA, amtB, amtInTok } = msg;

          if (!when) {
            return { when: false, msg: { amtA: 0, amtB: 0, calcK: 0, amtOut: 0, amtInTok: tokA }};
          } else {

            assume(amtInTok == tokA || amtInTok == tokB, "amtInTok == tokA or tokB");

            const balCheck = balance(tokA) > 0 && balance(tokB) > 0;
            if (amtInTok == tokA) {
              // in: A out: B
              assume(amtA > 0, "amtA > 0");
              assume(amtB == 0, "amtB == 0");
              const out = getAmtOut(amtA, balance(tokA), balance(tokB));
              assume(out <= balance(tokB), "out <= bal(tokB)");
              const kp = newK(amtA, tokA, out, tokB);
              assume(kp >= market.k, "kp == market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok }};
            } else {
              // in: B out: A
              assume(amtA == 0, "amtA == 0");
              assume(amtB > 0, "amtB > 0");
              const out = getAmtOut(amtB, balance(tokB), balance(tokA));
              assume(out <= balance(tokA), "out <= bal(tokA)");
              const kp = newK(amtB, tokB, out, tokA);
              assume(kp >= market.k, "kp == market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok }};
            }
          }
        }),
        (({ amtA, amtB }) => [ 0, [ 0, pool ], [ amtA, tokA], [ amtB, tokB ] ]),
        (({ amtA, amtB, calcK, amtOut, amtInTok }) => {
          require(calcK >= market.k, "require calcK == market.k");
          require(balance(tokA) > 0 && balance(tokB) > 0);

          // Transfer to Trader
          const currentTrader = this;
          if (amtInTok == tokA) {
            require(amtOut <= balance(tokB), "amtOut < bal(tokB)");
            transfer(amtOut, tokB).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtA, tokA, amtOut, tokB ]); });
          } else {
            require(amtOut <= balance(tokA), "amtOut < bal(tokA)");
            transfer(amtOut, tokA).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtB, tokB, amtOut, tokA ]); });
          }
          const marketP = { k: balance(tokA) * balance(tokB) };

          return [ true, marketP, poolMinted ];
        })
      )
      .timeout(1024, () => {
        Anybody.publish();
        return [ true, market, poolMinted ]; });

  commit();

  Admin.publish();
  pool.burn(balance(pool));
  if (!pool.destroyed()) {
    pool.destroy();
  }
  transfer(balance(tokA), tokA).to(Admin);
  transfer(balance(tokB), tokB).to(Admin);
  commit();

  exit();
});
