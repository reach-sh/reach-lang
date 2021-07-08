// Still cannot do dynamic computation of Tokens, so
// cannot write generically
'reach 0.1';

export const NUM_OF_TOKENS = 2;

const MUInt = Maybe(UInt);

const MkArray = (ty) =>
  Array(ty, NUM_OF_TOKENS)

export const main = Reach.App(() => {
  // Types
  const Market = Object({
    k: MUInt,
    tokens: MkArray(Object({ bal: UInt })),
  });

  const State = Tuple(Bool, Market);

  const Withdraw = Object({
    liquidity: UInt
  });

  const Deposit = Object({
    amtIns: MkArray(UInt),
    ratios: MkArray(UInt),
  });

  // Participants
  const Admin = Participant('Admin', {
    formulaValuation: UInt,
    tokA: Token,
    tokB: Token,
    shouldClosePool: Fun([State], Bool),
  });
  const Provider = ParticipantClass('Provider', {
    withdrawMaybe: Fun([State], Object({
      when: Bool,
      msg : Withdraw,
    })),
    withdrawDone: Fun([Bool, MkArray(UInt)], Null),
    depositMaybe: Fun([State], Object({
      when: Bool,
      msg: Deposit,
    })),
  });
  const Trader = ParticipantClass('Trader', {});

  deploy();

  Admin.only(() => {
    // const formulaValuation = declassify(interact.formulaValuation);
    const tokA = declassify(interact.tokA);
    const tokB = declassify(interact.tokB);
    assume(tokA != tokB);
  });
  Admin.publish(tokA, tokB);
  require(tokA != tokB);

  const initialMarket = {
    k: MUInt.None(),
    tokens: Array.replicate(NUM_OF_TOKENS, { bal: 0 }),
  };

  const totalSupply = 100;
  const pool = new Token({ supply: totalSupply });

  const constantProduct = (market) => {
    const mk = market.k;
    const x = balance(tokA);
    const y = balance(tokB);
    const cp = maybe(mk, true, (k) => k >= x * y);
    // const bc =
    //   market.tokens[0].bal == balance(tokA) &&
    //   market.tokens[1].bal == balance(tokB);
    return cp;
  };

  const [ alive, market, poolSupply ] =
    parallelReduce([ true, initialMarket, totalSupply ])
      .invariant(balance() == 0 && constantProduct(market))
      .while(alive)
      .paySpec([ pool, tokA, tokB ])
      .case(Admin,
        (() => ({
          when: alive && declassify(interact.shouldClosePool([ alive, market ]))
        })),
        (() => [ 0, [0, pool], [ 0, tokA ], [ 0, tokB ] ]),
        (() => {
          return [ false, market, poolSupply ]; })
      )
      .case(Provider,
        (() => {
          const { when, msg } = declassify(interact.withdrawMaybe([ alive, market ]));
          assume(msg.liquidity < poolSupply);
          return { when, msg };
        }),
        (({ liquidity }) => [ 0, [ liquidity, pool ], [ 0, tokA ], [ 0, tokB ] ]),
        (({ liquidity }) => {
          require(liquidity < poolSupply);

          // Balances have fees incorporated
          const balances = array(UInt, [balance(tokA), balance(tokB)]);

          // Amount of each token in reserve to return to Provider
          const amtOuts = balances.map(bal => liquidity * bal / poolSupply);

          // Payout provider
          transfer(amtOuts[0], tokA).to(this);
          transfer(amtOuts[1], tokB).to(this);

          // Burn the liquidity tokens
          pool.burn(liquidity);
          const poolSupplyP = poolSupply - liquidity;

          // Update market
          const marketP = {
            ...market,
            tokens: Array.zip(market.tokens, amtOuts)
                      .map(([ o, out ]) => ({ bal: o.bal - out })),
          }

          // Inform frontend of their payout
          const currentProvider = this;
          Provider.interact.withdrawDone(currentProvider == this, amtOuts);

          return [ true, marketP, poolSupplyP ];
        })
      )
      .case(Provider,
        (() => {
          const { when, msg } = declassify(interact.depositMaybe([ alive, market ]));
          return { when, msg };
        }),
        (({ amtIns }) => [0, [ 0, pool ], [ amtIns[0], tokA ], [ amtIns[1], tokB] ]),
        (({ amtIns, ratios }) => {

          // Temp satisfy verifier
          transfer(amtIns[0], tokA).to(this);
          transfer(amtIns[1], tokB).to(this);

          const ptoks = totalSupply == poolSupply
            // This is first deposit
            ? sqrt(amtIns.product(), 10)
            : Array.zip(market.tokens, amtIns)
                .map(([ o, inAmt ]) => (inAmt / o.bal) / poolSupply)
                .average();
          transfer(ptoks, pool).to(this);

          // const k = market.k.match({
          //   Some: ((i) => {

          //   }),
          //   None: (() => {
          //     return sqrt(x * y);
          //   }),
          // });

          // const marketP = {
          //   k,
          //   tokens: Array.zip(market.tokens, amtIns)
          //             .map(([ o, inAmt ]) => ({ bal: o.bal + inAmt })),
          // }

          return [ true, market, poolSupply ];
        })
      )
      .timeout(10, () => {
        Anybody.publish();
        return [ false, market, poolSupply ]; });

  pool.burn(balance(pool));
  transfer(balance(tokA), tokA).to(Admin);
  transfer(balance(tokB), tokB).to(Admin);
  commit();


});
