// Special case contract to create a pool
// between NETWORK - NON-NETWORK tokens.
// TokA is implicitly network
'reach 0.1';

import {
  avg,
  getAmtOut,
  mint,
  mkAdminInterface,
  MToken,
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

  Tokens.bTok.set(tokB);

  assert(balance() == 0);
  assert(balance(tokB) == 0);

  const initialMarket = {
    k: balance() * balance(tokB)
  };

  const totalSupply = UInt.max;
  const pool = new Token({ supply: totalSupply });

  commit();

  Provider.interact.acceptToken(pool);
  Trader.interact.acceptToken(pool);

  Admin.publish();

  const newK = (amtIn, balIn, amtOut, balOut) =>
    (balOut - amtOut) * (balIn + amtIn);

  assert(balance(pool) == totalSupply);

  const [ alive, market, poolMinted ] =
    parallelReduce([ true, initialMarket, 0 ])
      .define(() => {
        const st = [ alive, market ];
        Tokens.aBal.set(balance());
        Tokens.bBal.set(balance(tokB));
        const wrap = (f, onlyIfAlive) => {
          const { when, msg } = declassify(f(st));
          return { when: (declassify(onlyIfAlive) ? alive : true) && when, msg };
        }
        const constantProduct = () => {
          const x = balance();
          const y = balance(tokB);
          return x * y == market.k;
        };
      })
      .invariant(
        pool.supply() == totalSupply &&
        totalSupply == balance(pool) + poolMinted &&
        constantProduct())
      .while(alive || poolMinted > 0)
      .paySpec([ pool, tokB ])
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
            assume(balance() > 0 && balance(tokB) > 0, "bal(tokA) > 0 && bal(tokB) > 0");
            return { when, msg };
          } else {
            return { when: false, msg: { liquidity: 0 }};
          }
        }),
        (({ liquidity }) => [ 0, [ liquidity, pool ], [ 0, tokB ] ]),
        (({ liquidity }) => {
          require(poolMinted > 0);
          require(liquidity <= poolMinted, "liquidity <= poolMinted");
          require(balance() > 0 && balance(tokB) > 0, "bal(tokA) > 0 && bal(tokB) > 0");

          // Balances have fees incorporated
          const balances = array(UInt, [ balance(), balance(tokB) ]);

          // Amount of each token in reserve to return to Provider
          const amtOuts = balances.map(bal => liquidity * bal / poolMinted);

          // Payout provider
          const currentProvider = this;
          transfer(amtOuts[0]).to(currentProvider);
          transfer(amtOuts[1], tokB).to(currentProvider);

          // Update market
          const marketP = { k: balance() * balance(tokB) }

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
                ? sqrt((amtA / conUnit) * (amtB / conUnit), 4) * conUnit
                : avg( mint(amtA, balance(), poolMinted), mint(amtB, balance(tokB), poolMinted) );
            assume(minted < UInt.max, "minted < UInt.max");
            assume(minted > 0, "minted > 0");
            assume(minted < balance(pool), "assume minted < balance(pool)");
            return { when, msg: { amtA, amtB, minted } };
          } else {
            return { when: false, msg: { amtA: 0, amtB: 0, minted: 0 } };
          }
        }),
        (({ amtA, amtB }) => [ amtA, [ 0, pool ], [ amtB, tokB] ]),
        (({ amtA, amtB, minted }) => {
          require(minted > 0, "minted > 0");
          require(minted < balance(pool), "require minted < balance(pool)");

          // Pay liquidity provider their pool tokens
          const currentProvider = this;
          transfer(minted, pool).to(currentProvider);

          // Update market
          const marketP = { k: balance() * balance(tokB) }

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
            return { when: false, msg: { amtA: 0, amtB: 0, calcK: 0, amtOut: 0, amtInTok: MToken.None() }};
          } else {

            assume(maybe(amtInTok, true, (t) => t == tokB), "amtInTok == tokA or tokB");

            const balCheck = balance() > 0 && balance(tokB) > 0;
            if (isNone(amtInTok)) {
              // in: A out: B
              assume(amtA > 0, "amtA > 0");
              assume(amtB == 0, "amtB == 0");
              const out = getAmtOut(amtA, balance(), balance(tokB));
              assume(out <= balance(tokB), "out <= bal(tokB)");
              const kp = newK(amtA, balance(), out, balance(tokB));
              assume(kp >= market.k, "kp == market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok }};
            } else {
              // in: B out: A
              assume(amtA == 0, "amtA == 0");
              assume(amtB > 0, "amtB > 0");
              const out = getAmtOut(amtB, balance(tokB), balance());
              assume(out <= balance(), "out <= bal(tokA)");
              const kp = newK(amtB, balance(tokB), out, balance());
              assume(kp >= market.k, "kp == market.k");
              return { when: balCheck, msg: { amtA, amtB, calcK: kp, amtOut: out, amtInTok }};
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
            require(amtOut <= balance(tokB), "amtOut < bal(tokB)");
            transfer(amtOut, tokB).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtA, MToken.None(), amtOut, MToken.Some(tokB) ]); });
          } else {
            require(amtOut <= balance(), "amtOut < bal(tokA)");
            transfer(amtOut).to(currentTrader);

            // Inform frontend of their deposit
            Trader.only(() => {
              interact.tradeDone(currentTrader == this, [ amtB, MToken.Some(tokB), amtOut, MToken.None() ]); });
          }
          const marketP = { k: balance() * balance(tokB) };

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
