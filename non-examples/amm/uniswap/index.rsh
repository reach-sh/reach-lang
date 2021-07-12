// Still cannot do dynamic computation of Tokens, so
// cannot write generically
'reach 0.1';

export const NUM_OF_TOKENS = 2;

const MUInt = Maybe(UInt);

const MkArray = (ty) =>
  Array(ty, NUM_OF_TOKENS);

export const getReserves = (market) => market.tokens;

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
    amtIns: MkArray(UInt),
    ratios: MkArray(UInt),
  });

  // Participants
  const Admin = Participant('Admin', {
    formulaValuation: UInt,
    tokA: Token,
    tokB: Token,
    shouldClosePool: Fun([State], Object({
      when: Bool,
      msg : Null,
    })),
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

  assert(balance(tokA) == 0, "Starting with 0 balance of TokA");
  assert(balance(tokB) == 0, "Starting with 0 balance of TokB");
  const initialMarket = {
    k: balance(tokA) * balance(tokB)
  };

  const totalSupply = UInt.max;
  const pool = new Token({ supply: totalSupply });

  const mint = (inp, b) => ( inp / b ) * pool.supply();
  const firstMint = (inA, inB) => sqrt(inA * inB, 10);

  const [ alive, market ] =
    parallelReduce([ true, initialMarket ])
      .define(() => {
        const st = [ alive, market ];
        const wrap = (f, onlyIfAlive) => {
          const { when, msg } = declassify(f(st));
          return { when: (declassify(onlyIfAlive) ? alive : true) && when, msg };
        }
        const constantProduct = (m) => {
          const x = balance(tokA);
          const y = balance(tokB);
          return x * y == m.k;
        };
      })
      .invariant(
        balance() == 0 &&
        constantProduct(market)
      )
      .while(alive)
      .paySpec([ pool, tokA, tokB ])
      .case(Admin,
        (() => wrap(interact.shouldClosePool, true)),
        (() => [ 0, [0, pool], [ 0, tokA ], [ 0, tokB ] ]),
        (() => {
          return [ false, market ]; })
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
          transfer(amtOuts[0], tokA).to(this);
          transfer(amtOuts[1], tokB).to(this);

          // Burn the liquidity tokens
          pool.burn(liquidity);

          const k = balance(tokA) * balance(tokB);

          // Update market
          const marketP = { k }

          // Inform frontend of their payout
          const currentProvider = this;
          Provider.interact.withdrawDone(currentProvider == this, amtOuts);

          return [ true, marketP ];
        })
      )
      .case(Provider,
        (() => {
          const { when, msg } = wrap(interact.depositMaybe, true);
          const { amtIns } = msg;
          if (market.k > 0) {
            assume(mint(amtIns[0], balance(tokA)) < balance(pool));
            assume(mint(amtIns[1], balance(tokB)) < balance(pool));
          } else {
            assume(firstMint(amtIns[0], amtIns[1]) < balance(pool));
          }

          return { when, msg };
        }),
        (({ amtIns }) => [0, [ 0, pool ], [ amtIns[0], tokA ], [ amtIns[1], tokB] ]),
        (({ amtIns, ratios }) => {

          const inA = amtIns[0];
          const inB = amtIns[1];

          const f = (inp, b) => {
            assert(inp <= b, "amtIn <= balance(tok)");
            const res = mint(inp, b);
            require(res < balance(pool), "liquidity minted less than balance");
            return res;
          }

          const sMinted = market.k == 0
            // If first deposit, use geometric mean
            ? firstMint(inA, inB)
            : min( f(inA, balance(tokA)), f(inB, balance(tokB)) );

          // require(sMinted < balance(pool), "liquidity minted less than balance");
          // This needs to be `require`d, but does not halt
          if (sMinted < balance(pool)) {
            transfer(sMinted, pool).to(this);
          }

          // Update K
          const k = balance(tokA) * balance(tokB);

          const marketP = { k };

          return [ true, marketP ];
        })
      )
      .timeout(10, () => {
        Anybody.publish();
        return [ false, market ]; });

  pool.burn(balance(pool));
  transfer(balance(tokA), tokA).to(Admin);
  transfer(balance(tokB), tokB).to(Admin);
  commit();


});
