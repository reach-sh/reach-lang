// Still cannot do dynamic computation of Tokens, so cannot write generically
'reach 0.1';

export const NUM_OF_TOKENS = 2;

const MkArray = (ty) => Array(ty, NUM_OF_TOKENS);

const min = (a, b) => a < b ? a : b;

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
    amtB: UInt
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
    ...ProviderInterface,
    tokA: Token,
    tokAAmt: UInt,
    tokB: Token,
    tokBAmt: UInt,
    shouldClosePool: Fun([State], Object({
      when: Bool,
      msg : Null,
    })),
  });

  const Trader = ParticipantClass('Trader', {
    tradeMaybe: Fun([State], Object({
      when: Bool,
      msg : Trade,
    })),
    tradeDone: Fun([Bool, Tuple(UInt, UInt)], Null),
  });

  deploy();

  // Admin sets up initial pool by making first deposit
  Admin.only(() => {
    const tokA = declassify(interact.tokA);
    const tokB = declassify(interact.tokB);
    const tokAAmt = declassify(interact.tokAAmt);
    const tokBAmt = declassify(interact.tokBAmt);
    assume(tokA != tokB);
    assume(tokAAmt > 0 && tokBAmt > 0);
  });

  Admin.publish(tokA, tokB, tokAAmt, tokBAmt)
    .pay([ [ tokAAmt, tokA ], [ tokBAmt, tokB ] ]);

  require(tokA != tokB);
  require(tokAAmt > 0 && tokBAmt > 0);

  const initialMarket = {
    k: balance(tokA) * balance(tokB)
  };

  const totalSupply = UInt.max;
  const pool = new Token({ supply: totalSupply });

  // Calculates how much LP tokens to mint
  const mint = (amtIn, bal) => ( amtIn / bal ) * pool.supply();

  const [ alive, market ] =
    parallelReduce([ true, initialMarket ])
      .define(() => {
        const st = [ alive, market ];
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
      .invariant(balance() == 0 && constantProduct())
      .while(alive)
      .paySpec([ pool, tokA, tokB ])
      .case(Admin,
        (() => wrap(interact.shouldClosePool, true)),
        ((_) => [ 0, [0, pool], [ 0, tokA ], [ 0, tokB ] ]),
        (() => { return [ false, market ]; })
      )
      .case(Provider,
        (() => {
          const { when, msg } = declassify(interact.withdrawMaybe(st));
          assume(msg.liquidity < pool.supply());
          return { when, msg };
        }),
        (({ liquidity }) => [ 0, [ liquidity, pool ], [ 0, tokA ], [ 0, tokB ] ]),
        (({ liquidity }) => {
          require(liquidity < pool.supply());

          // Balances have fees incorporated
          const balances = array(UInt, [ balance(tokA), balance(tokB) ]);

          // Amount of each token in reserve to return to Provider
          const amtOuts = balances.map(bal => liquidity * bal / pool.supply());

          // Payout provider
          const currentProvider = this;
          transfer(amtOuts[0], tokA).to(currentProvider);
          transfer(amtOuts[1], tokB).to(currentProvider);

          // Burn the liquidity tokens
          pool.burn(liquidity);

          // Update market
          const marketP = { k: balance(tokA) * balance(tokB) }

          // Inform frontend of their payout
          Provider.interact.withdrawDone(currentProvider == this, amtOuts);

          return [ true, marketP ];
        })
      )
      .case(Provider,
        (() => {
          const { when, msg } = wrap(interact.depositMaybe, true);
          const { amtA, amtB } = msg;

          // Ensure minted amount is less than pool balance
          interact.log("amtA", amtA);
          interact.log("balance(tokA)", balance(tokA));
          interact.log("mintA", mint(amtA, balance(tokA)));
          interact.log("amtB", amtB);
          interact.log("balance(tokB)", balance(tokB));
          interact.log("mintB", mint(amtB, balance(tokB)));
          const minted =
            min( mint(amtA, balance(tokA)), mint(amtB, balance(tokB) ));

          assume(minted > 0, "minted > 0");
          assume(minted < balance(pool), "assume minted < balance(pool)");

          return { when, msg: { amtA, amtB, minted } };
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
          Provider.interact.depositDone(currentProvider == this, amtA, amtB, minted);

          return [ true, marketP ];
        })
      )
      .case(Trader,
        (() => {
          const { when, msg } = wrap(interact.tradeMaybe, true);
          const { amtA, amtB } = msg;

          const newK = (amtIn, amtInTok, amtOut, amtOutTok) =>
            (balance(amtOutTok) - amtOut) * (balance(amtInTok) + amtIn);

          // Traders pay 1 type of coin
          const calcK = (amtA == 0)
            ? (() => {
                assume(amtB > 0);
                // amtIn = amtB, amtOut = amtA
                return newK(amtB, tokB, amtA, tokA); })()
            : (() => {
                assume(amtB == 0);
                // amtIn = amtA, amtOut = amtB
                return newK(amtA, tokA, amtB, tokB); })();
          assume(calcK == market.k);

          return { when, msg: { amtA, amtB, calcK } }
        }),
        (({ amtA, amtB }) => [ 0, [ 0, pool ], [ amtA, tokA], [ amtB, tokB ] ]),
        (({ amtA, amtB, calcK }) => {
          require(calcK == market.k);

          // Transfer to Trader
          const currentTrader = this;
          transfer(amtA, tokA).to(currentTrader);
          transfer(amtB, tokB).to(currentTrader);

          // Inform frontend of their deposit
          Trader.interact.tradeDone(currentTrader == this, [ amtA, amtB ]);

          return [ true, market ];
        })
      )
      .timeout(1024, () => {
        Anybody.publish();
        return [ false, market ]; });

  pool.burn(balance(pool));
  transfer(balance(tokA), tokA).to(Admin);
  transfer(balance(tokB), tokB).to(Admin);
  commit();
});
