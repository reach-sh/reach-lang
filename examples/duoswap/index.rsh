'reach 0.1';

import {
  avg,
  getAmtOut,
  min,
  mint,
  mkAdminInterface,
  MToken,
  muldiv,
  noop,
  ProviderInterface,
  TraderInterface,
  TokensView
} from './util.rsh';

export const main = Reach.App(() => {

  const Provider = ParticipantClass('Provider', ProviderInterface);
  const Admin = Participant('Admin', mkAdminInterface(true));
  const Trader = ParticipantClass('Trader', TraderInterface);
  const Tokens = View('Tokens', TokensView);

  deploy();

  // Admin sets up initial pool by making first deposit
  Admin.only(() => {
    const tokA = declassify(interact.tokA);
    const tokB = declassify(interact.tokB);
    const conUnit = declassify(interact.conUnit);
    assume(UInt.max > 0);
    assume(tokA != tokB);
    assume(conUnit > 0);
  });

  Admin.publish(tokA, tokB, conUnit);

  require(UInt.max > 0);
  require(tokA != tokB);
  require(conUnit > 0);

  Tokens.aTok.set(tokA);
  Tokens.bTok.set(tokB);

  assert(balance(tokA) == 0);
  assert(balance(tokB) == 0);

  const initialMarket = {
    k: (balance(tokA) / conUnit) * (balance(tokB) / conUnit)
  };

  const totalSupply = UInt.max;
  const pool = new Token({ supply: totalSupply });

  commit();

  Provider.interact.acceptToken(pool);
  Trader.interact.acceptToken(pool);

  Admin.publish();

  const newK = (amtIn, ainTok, amtOut, amtOutTok) =>
    ((balance(amtOutTok) - amtOut) / conUnit) * ((balance(ainTok) + amtIn) / conUnit);

  assert(balance(pool) == totalSupply);

  const [ alive, market, poolMinted ] =
    parallelReduce([ true, initialMarket, 0 ])
      .define(() => {
        const st = [ alive, market ];
        Tokens.aBal.set(balance(tokA));
        Tokens.bBal.set(balance(tokB));
        const constantProduct = () => {
          return (balance(tokA) / conUnit) * (balance(tokB) / conUnit) == market.k;
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
        (() => declassify(interact.shouldClosePool(st))),
        (() => { return [ false, market, poolMinted ]; })
      )
      .case(Provider,
        (() => {
          const { when, msg } = (poolMinted > 0)
            ? declassify(interact.withdrawMaybe(st))
            : { when: false, msg: {liquidity: 0 }};

          if (!when) {
            return { when: false, msg };
          } else {
            assume(poolMinted > 0);
            assume(msg.liquidity <= poolMinted, "liquidity <= poolMinted");
            assume(balance(tokA) > 0 && balance(tokB) > 0, "bal(tokA) > 0 && bal(tokB) > 0");
            assume(poolMinted > conUnit, "poolMinted > conUnit");
            return { when: true, msg };
          }
        }),
        (({ liquidity }) => [ 0, [ liquidity, pool ], [ 0, tokA ], [ 0, tokB ] ]),
        (({ liquidity }) => {
          require(poolMinted > 0, "poolMinted > 0");
          require(liquidity <= poolMinted, "liquidity <= poolMinted");
          require(balance(tokA) > 0 && balance(tokB) > 0, "bal(tokA) > 0 && bal(tokB) > 0");

          // Balances have fees incorporated
          const balances = array(UInt, [ balance(tokA), balance(tokB) ]);

          // Amount of each token in reserve to return to Provider
          require(poolMinted > conUnit, "poolMinted > conUnit");
          const amtOuts = balances.map(bal =>
            min(bal, muldiv(liquidity, bal, poolMinted, conUnit, noop)));

          // Payout provider
          const currentProvider = this;
          transfer(amtOuts[0], tokA).to(currentProvider);
          transfer(amtOuts[1], tokB).to(currentProvider);

          // Update market
          const marketP = { k: (balance(tokA) / conUnit) * (balance(tokB) / conUnit) }

          // Inform frontend of their payout
          Provider.only(() => {
            interact.withdrawDone(currentProvider == this, amtOuts) });

          return [ true, marketP, poolMinted - liquidity ];
        })
      )
      .case(Provider,
        (() => {
          const { when, msg } = alive
            ? declassify(interact.depositMaybe(st))
            : { when: false, msg: { amtA: 0, amtB: 0 }} ;

          if (!when) {
            return { when: false, msg: { ...msg, minted: 0 } };
          } else {
            const { amtA, amtB } = msg;
            const minted =
              (poolMinted == 0)
                ? sqrt((amtA / conUnit) * (amtB / conUnit), 4) * conUnit
                : avg( mint(amtA, balance(tokA), poolMinted, conUnit), mint(amtB, balance(tokB), poolMinted, conUnit) );
            assume(minted < UInt.max, "minted < UInt.max");
            assume(minted > 0, "minted > 0");
            assume(minted < balance(pool), "minted < balance(pool)");
            return { when: true, msg: { amtA, amtB, minted } };
          }
        }),
        (({ amtA, amtB }) => [0, [ 0, pool ], [ amtA, tokA ], [ amtB, tokB] ]),
        (({ amtA, amtB, minted }) => {
          require(minted < UInt.max, "minted < UInt.max");
          require(minted > 0, "minted > 0");
          require(minted < balance(pool), "require minted < balance(pool)");

          // Pay liquidity provider their pool tokens
          const currentProvider = this;
          transfer(minted, pool).to(currentProvider);

          // Update market
          const marketP = { k: (balance(tokA) / conUnit) * (balance(tokB) / conUnit) }

          // Inform frontend of their deposit
          Provider.only(() => {
            interact.depositDone(currentProvider == this, amtA, amtB, minted) });

          return [ true, marketP, poolMinted + minted ];
        })
      )
      .case(Trader,
        (() => {
          const { when, msg } = alive ? declassify(interact.tradeMaybe(st))
            : { when: false, msg: { amtA: 0, amtB: 0, amtInTok: MToken.None() } };
          const { amtA, amtB, amtInTok } = msg;

          if (!when || market.k == 0) {
            return { when: false, msg: { amtA: 0, amtB: 0, calcK: 0, amtOut: 0, amtInTok: tokA }};
          } else {

            assume(amtInTok == MToken.Some(tokA) || amtInTok == MToken.Some(tokB), "amtInTok == tokA or tokB");

            const balCheck = balance(tokA) > 0 && balance(tokB) > 0;
            assume(balCheck, "balance(tokA) > 0 && balance(tokB) > 0");
            if (amtInTok == MToken.Some(tokA)) {
              // in: A out: B
              assume(amtA > 0, "amtA > 0");
              assume(amtB == 0, "amtB == 0");
              const out = getAmtOut(amtA, balance(tokA), balance(tokB), conUnit);
              assume(out <= balance(tokB), "out <= bal(tokB)");
              const kp = newK(amtA, tokA, out, tokB);
              assume(kp >= market.k, "kp >= market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok: tokA }};
            } else {
              // in: B out: A
              assume(amtA == 0, "amtA == 0");
              assume(amtB > 0, "amtB > 0");
              const out = getAmtOut(amtB, balance(tokB), balance(tokA), conUnit);
              assume(out <= balance(tokA), "out <= bal(tokA)");
              const kp = newK(amtB, tokB, out, tokA);
              assume(kp >= market.k, "kp >= market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok: tokB }};
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
              interact.tradeDone(currentTrader == this, [ amtA, MToken.Some(tokA), amtOut,  MToken.Some(tokB) ]); });
          } else {
            require(amtOut <= balance(tokA), "amtOut < bal(tokA)");
            transfer(amtOut, tokA).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtB,  MToken.Some(tokB), amtOut,  MToken.Some(tokA) ]); });
          }
          const marketP = { k: (balance(tokA) / conUnit) * (balance(tokB) / conUnit) };

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
