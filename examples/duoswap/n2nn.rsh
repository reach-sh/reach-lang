// Special case contract to create a pool
// between NETWORK - NON-NETWORK tokens.
// TokA is implicitly network
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
  const Admin = Participant('Admin', mkAdminInterface(false));
  const Trader = ParticipantClass('Trader', TraderInterface);
  const Tokens = View('Tokens', TokensView);

  deploy();

  // Admin sets up initial pool by making first deposit
  Admin.only(() => {
    const tokB = declassify(interact.tokB);
    const conUnit = declassify(interact.conUnit);
    assume(UInt.max > 0);
    assume(conUnit > 0);
  });

  Admin.publish(tokB, conUnit);

  require(UInt.max > 0);
  require(conUnit > 0);

  Tokens.bTok.set(tokB);

  assert(balance() == 0);
  assert(balance(tokB) == 0);

  const initialMarket = {
    k: (balance() / conUnit) * (balance(tokB) / conUnit)
  };

  const totalSupply = UInt.max;
  const pool = new Token({ supply: totalSupply });

  commit();

  Provider.interact.acceptToken(pool);
  Trader.interact.acceptToken(pool);

  Admin.publish();

  const newK = (amtIn, balIn, amtOut, balOut) =>
    ((balOut - amtOut) / conUnit) * ((balIn + amtIn) / conUnit);

  assert(balance(pool) == totalSupply);

  const [ alive, market, poolMinted ] =
    parallelReduce([ true, initialMarket, 0 ])
      .define(() => {
        const st = [ alive, market ];
        Tokens.aBal.set(balance());
        Tokens.bBal.set(balance(tokB));
        const constantProduct = () => {
          return (balance() / conUnit) * (balance(tokB) / conUnit) == market.k;
        };
      })
      .invariant(
        pool.supply() == totalSupply &&
        totalSupply == balance(pool) + poolMinted &&
        constantProduct())
      .while(alive || poolMinted > 0)
      .paySpec([ pool, tokB ])
      .case(Admin,
        (() => declassify(interact.shouldClosePool(st))),
        (() => { return [ false, market, poolMinted ]; })
      )
      .case(Provider,
        (() => {
          const { when, msg } = (poolMinted > 0)
            ? declassify(interact.withdrawMaybe(st))
            : { when: false, msg: { liquidity: 0 } };

          if (!when) {
            return { when: false, msg };
          } else {
            assume(poolMinted > 0);
            assume(msg.liquidity <= poolMinted, "liquidity <= poolMinted");
            assume(balance() > 0 && balance(tokB) > 0, "bal() > 0 && bal(tokB) > 0");
            assume(poolMinted > conUnit, "poolMinted > conUnit");
            return { when: true, msg };
          }
        }),
        (({ liquidity }) => [ 0, [ liquidity, pool ], [ 0, tokB ] ]),
        (({ liquidity }) => {
          require(poolMinted > 0, "poolMinted > 0");
          require(liquidity <= poolMinted, "liquidity <= poolMinted");
          require(balance() > 0 && balance(tokB) > 0, "bal() > 0 && bal(tokB) > 0");

          // Balances have fees incorporated
          const balances = array(UInt, [ balance(), balance(tokB) ]);

          // Amount of each token in reserve to return to Provider
          require(poolMinted > conUnit, "poolMinted > conUnit");
          const amtOuts = balances.map(bal =>
            min(bal, muldiv(liquidity, bal, poolMinted, conUnit, noop)));

          // Payout provider
          const currentProvider = this;
          transfer(amtOuts[0]).to(currentProvider);
          transfer(amtOuts[1], tokB).to(currentProvider);

          // Update market
          const marketP = { k: (balance() / conUnit) * (balance(tokB) / conUnit) };

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
                : avg( mint(amtA, balance(), poolMinted, conUnit), mint(amtB, balance(tokB), poolMinted, conUnit) );
            assume(minted < UInt.max, "minted < UInt.max");
            assume(minted > 0, "minted > 0");
            assume(minted < balance(pool), "minted < balance(pool)");
            return { when: true, msg: { amtA, amtB, minted } };
          }
        }),
        (({ amtA, amtB }) => [ amtA, [ 0, pool ], [ amtB, tokB] ]),
        (({ amtA, amtB, minted }) => {
          require(minted < UInt.max, "minted < UInt.max");
          require(minted > 0, "minted > 0");
          require(minted < balance(pool), "require minted < balance(pool)");

          // Pay liquidity provider their pool tokens
          const currentProvider = this;
          transfer(minted, pool).to(currentProvider);

          // Update market
          const marketP = { k: (balance() / conUnit) * (balance(tokB) / conUnit) }

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
            return { when: false, msg: { amtA: 0, amtB: 0, calcK: 0, amtOut: 0, amtInTok: MToken.None() }};
          } else {

            assume(maybe(amtInTok, true, (t) => t == tokB), "amtInTok == tokA or tokB");

            const balCheck = balance() > 0 && balance(tokB) > 0;
            assume(balCheck, "balance() > 0 && balance(tokB) > 0")
            if (isNone(amtInTok)) {
              // in: A out: B
              assume(amtA > 0, "amtA > 0");
              assume(amtB == 0, "amtB == 0");
              const out = getAmtOut(amtA, balance(), balance(tokB), conUnit);
              assume(out <= balance(tokB), "out <= bal(tokB)");
              const kp = newK(amtA, balance(), out, balance(tokB));
              assume(kp >= market.k, "kp >= market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok: MToken.None() }};
            } else {
              // in: B out: A
              assume(amtA == 0, "amtA == 0");
              assume(amtB > 0, "amtB > 0");
              const out = getAmtOut(amtB, balance(tokB), balance(), conUnit);
              assume(out <= balance(), "out <= bal(tokA)");
              const kp = newK(amtB, balance(tokB), out, balance());
              assume(kp >= market.k, "kp >= market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok: MToken.Some(tokB) }};
            }
          }
        }),
        (({ amtA, amtB }) => [ amtA, [ 0, pool ], [ amtB, tokB ] ]),
        (({ amtA, amtB, calcK, amtOut, amtInTok }) => {
          require(calcK >= market.k, "require calcK == market.k");
          require(balance() > 0 && balance(tokB) > 0);

          // Transfer to Trader
          const currentTrader = this;
          if (isNone(amtInTok)) {
            require(amtOut <= balance(tokB), "amtOut <= balance(tokB)");
            transfer(amtOut, tokB).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtA, MToken.None(), amtOut, MToken.Some(tokB) ]); });
          } else {
            require(amtOut <= balance(), "amtOut <= balance()");
            transfer(amtOut).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtB, MToken.Some(tokB), amtOut, MToken.None() ]); });
          }
          const marketP = { k: (balance() / conUnit) * (balance(tokB) / conUnit) };

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
  transfer(balance()).to(Admin);
  transfer(balance(tokB), tokB).to(Admin);
  commit();

  exit();
});
