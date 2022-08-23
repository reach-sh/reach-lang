"reach 0.1";

//"use strict";

const BIG_NUMBER = 1_000_000_000;
const ALGO_TO_CREATE = 100;

const InitialStateObj = {
  startBlock: UInt,
  subscriptionEndBlock: UInt,
  unlockStartBlock: UInt,
  unlockEndBlock: UInt,

  token: Token,
  tokenName: Bytes(32),
  tokenSymbol: Bytes(8),
  tokenSupply: UInt,
  tokenDecimals: UInt,

  wALGO: Token,
  lpToken: Token,
  liquidityPool: Contract,
  tokenAmount: UInt,

  metaLabsAddress: Address,
};

const InitialState = Struct([
  ["startBlock", UInt],
  ["subscriptionEndBlock", UInt],
  ["unlockStartBlock", UInt],
  ["unlockEndBlock", UInt],

  ["token", Token],
  ["tokenName", Bytes(32)],
  ["tokenSymbol", Bytes(8)],
  ["tokenSupply", UInt],
  ["tokenDecimals", UInt],

  ["wALGO", Token],
  ["lpToken", Token],
  ["liquidityPool", Contract],
  ["tokenAmount", UInt],

  ["metaLabsAddress", Address],
]);

const [
  isPhase,
  INITIAL,
  SUBSCRIPTION,
  RUNNING,
  WITHDRAW_LIQUIDITY,
  DAO_UNLOCK,
  CONFISCATE,
  MANUAL_SETTLEMENT,
  CLAIM,
  TERMINAL,
] = makeEnum(9);

const GlobalState = Struct([
  ["currentK", UInt], // From the dex
  ["totalTokenProvided", UInt],
  ["totalAlgoProvided", UInt],
  ["currentPhase", UInt],
]);

export const main = Reach.App(() => {
  setOptions({
    untrustworthyMaps: false,
    connectors: [ALGO, ETH],
  });

  const Common = {
    ...hasConsoleLogger,
    deployed: Fun([], Null),
  };

  const Creator = Participant("Creator", {
    ...Common,
    getParams: Fun(
      [],
      Object({
        ...InitialStateObj,
      })
    ),
  });

  const User = Participant("User", {
    ...Common,
  });

  void User;

  const State = View({
    initial: InitialState,
    global: GlobalState,
  });

  const Api = API({
    // arg: amount of wALGO to provide
    addLiquidity: Fun([UInt], Null),
    withdrawExcess: Fun([], Null),
    withdrawLiquidity: Fun([], Null),
    // arg: amount of token to repay
    unlock: Fun([UInt], Null),
    confiscate: Fun([], Null),
    // arg: how much divergent loss to cover
    payAndRelease: Fun([UInt], Null),
    daoClaim: Fun([], Null),
    // arg: amount of cALGO returned by LP
    lpClaim: Fun([UInt], Null),
  });

  const Event = Events({
    // description, number
    logNumber: [Bytes(32), UInt],
  });

  init();

  // Initialize pool
  Creator.only(() => {
    const {
      startBlock,
      subscriptionEndBlock,
      unlockStartBlock,
      unlockEndBlock,
      token,
      tokenName,
      tokenSymbol,
      tokenSupply,
      tokenDecimals,
      wALGO,
      lpToken,
      liquidityPool,
      tokenAmount,
      metaLabsAddress,
    } = declassify(interact.getParams());

    assume(distinct(token, wALGO, lpToken));

    assume(startBlock < subscriptionEndBlock);
    assume(subscriptionEndBlock < unlockStartBlock);
    assume(unlockStartBlock < unlockEndBlock);

    assume(tokenAmount > 0);
  });

  Creator.publish(
    startBlock,
    subscriptionEndBlock,
    unlockStartBlock,
    unlockEndBlock,
    token,
    tokenName,
    tokenSymbol,
    tokenSupply,
    tokenDecimals,
    wALGO,
    lpToken,
    liquidityPool,
    tokenAmount,
    metaLabsAddress
  );

  commit();
  Creator.pay([ALGO_TO_CREATE, [tokenAmount, token]]);

  // TODO: better description, shall be provided from creator
  const cALGO = new Token({
    name: tokenName,
    symbol: tokenSymbol,
    url: Bytes(96).pad("https://cometa.farm"),
    supply: tokenSupply,
    decimals: tokenDecimals,
  });

  State.initial.set(
    InitialState.fromObject({
      startBlock,
      subscriptionEndBlock,
      unlockStartBlock,
      unlockEndBlock,
      token,
      tokenName,
      tokenSymbol,
      tokenSupply,
      tokenDecimals,
      wALGO,
      lpToken,
      liquidityPool,
      tokenAmount,
      metaLabsAddress,
    })
  );

  const NInt = Refine(UInt, (x) => x > 0);

  const liquidityPoolCtc = remote(liquidityPool, {
    addLiquidity: Fun([UInt], Null),
    addLiquidity2: Fun([UInt, UInt], Null),
    removeLiquidity: Fun([UInt], Null),
    getLpPrice: Fun([], Tuple(UInt, UInt)),
    getK: Fun([], UInt),
  });

  each([Creator, User], () => {
    interact.deployed();
  });

  /*
   * SUBSCRIPTION
   */
  const mintTokens = (who, toMint) => {
    // The condition is always true in practice (for ALGO) but
    // we need it to make the validator happy.
    if (toMint <= balance(cALGO)) {
      transfer([[toMint, cALGO]]).to(who);
    }
  };

  // Assumes wALGO/token pool (wALGO is the first in pair).
  // But it's just for toy dex implementation, things will be very different in practice.
  const tryToProvideLiquidity = () => {
    const [algoReceived, [lp, wAlgoReminder, tokenReminder], retval] =
      liquidityPoolCtc.addLiquidity2
        .pay([0, [balance(wALGO), wALGO], [balance(token), token], [0, lpToken]])
        .withBill([lpToken, wALGO, token])(balance(wALGO), balance(token));
  };

  const [timeRemaining, hasTime] = makeDeadline(subscriptionEndBlock - thisConsensusTime());
  const [initialK, totalAlgoProvided] = parallelReduce([1, 0])
    .invariant(true)
    // TODO: is it possible that we will always have some dust wALGO?
    .while(hasTime() && balance(token) > 0)
    .paySpec([wALGO])
    .api_(
      Api.addLiquidity,

      (amount) => {
        check(thisConsensusTime() >= startBlock, "subscription period has not started yet");

        return [
          [0, [amount, wALGO]],
          (callback) => {
            tryToProvideLiquidity();
            mintTokens(this, amount);

            callback(null);
            return [liquidityPoolCtc.getK(), totalAlgoProvided + amount];
          },
        ];
      }
    )
    .timeRemaining(timeRemaining());

  /*
   * WITHDRAW EXCESS
   */
  // TODO shall probably be just fork?
  // TODO makeDeadline uses lastConsensusTime
  const [timeRemaining2, hasTime2] = makeDeadline(unlockStartBlock - thisConsensusTime());

  const [b] = parallelReduce([0])
    .invariant(true)
    .while(hasTime2() && balance(token) > 0)
    .api_(Api.withdrawExcess, () => {
      check(this == Creator);

      return [
        (callback) => {
          callback(null);
          transfer([[balance(token), token]]).to(this);
          return [b];
        },
      ];
    })
    .timeRemaining(timeRemaining2());

  /*
   * WITHDRAW LIQUIDITY
   */
  // Assumption: somebody will press this button relatively quickly.
  const [liqWithdrawn, algoToRedeemByDao, algoToRedeemByLp] = parallelReduce([false, 0, 0])
    .invariant(true)
    .while(!liqWithdrawn)
    .api(Api.withdrawLiquidity, (callback) => {
      callback(null);

      const currentK = liquidityPoolCtc.getK();

      // TODO: simplify?
      const [algoRecv, [wAlgoRecv, tokenRecv], retval] = liquidityPoolCtc.removeLiquidity
        .pay([[balance(lpToken), lpToken]])
        .withBill([wALGO, token])(balance(lpToken));

      // TODO: possible overflows, probably will need uint256 (and check if it's enough)
      const tradingFeesRatio = (BIG_NUMBER * currentK) / initialK;

      const tradingFeesAlgo =
        (totalAlgoProvided * (tradingFeesRatio - BIG_NUMBER) * 2) / BIG_NUMBER;

      const algoLpShouldHave = totalAlgoProvided + tradingFeesAlgo;

      // TODO more stuff here and save somehow?
      if (balance(wALGO) >= algoLpShouldHave) {
        const algoSurplus = balance(wALGO) - algoLpShouldHave;
        // TODO fees
        return [true, algoSurplus, algoLpShouldHave];
      } else {
        const algoDivergentLoss = algoLpShouldHave - balance(wALGO);
        return [true, 0, algoLpShouldHave];
      }
    });

  const [timeRemaining3, hasTime3] = makeDeadline(unlockEndBlock - thisConsensusTime());
  const [fullUnlock] = parallelReduce([algoToRedeemByLp >= balance(wALGO)])
    .invariant(true)
    .while(hasTime3() && !fullUnlock)
    .api_(Api.unlock, (amount) => {
      check(this == Creator);
      // TODO: make it possible to repay partially
      check(amount == algoToRedeemByLp - balance(wALGO));

      return [
        [[amount, token]],
        (callback) => {
          callback(null);
          transfer([[balance(token), token]]).to(this);
          return [true];
        },
      ];
    })
    .timeRemaining(timeRemaining3());

  /*
   * CONFISCATE
   */

  const [needToConfiscate ] = parallelReduce([!fullUnlock])
    .invariant(true)
    .while(needToConfiscate)
    .api_(Api.confiscate, () => {
      check(this == metaLabsAddress);

      return [
        (callback) => {
          callback(null);
          transfer([[balance(token), token]]).to(this);
          return [false];
        },
      ];
    });

  /*
   * PAY AND RELEASE
   */
  // TODO: should be initialized with !fullUnlock but it causes error
  //       https://github.com/reach-sh/reach-lang/issues/1183
  const [needToPay] = parallelReduce([!fullUnlock])
    .paySpec([wALGO])
    .invariant(true)
    .while(needToPay)
    .api_(Api.payAndRelease, (amount) => {
      check(this == metaLabsAddress);

      return [
        [0, [amount, wALGO]],
        (callback) => {
          callback(null);
          return [false];
        },
      ];
    });

  /*
   * CLAIM
   */
  const [totalBalance] = parallelReduce([balance(wALGO)])
    .paySpec([cALGO])
    .invariant(true)
    .while(true)
    .api_(Api.daoClaim, () => {
      check(this == Creator);

      return [
        (callback) => {
          callback(null);
          // TODO/ make it work with validator
          if (algoToRedeemByDao <= balance(wALGO)) {
            transfer([
              [algoToRedeemByDao, wALGO],
              [balance(token), token],
            ]).to(this);
          }
          return [totalBalance];
        },
      ];
    })
    .api(
      Api.lpClaim,
      (amount) => {},
      (amount) => [0, [amount, cALGO]],
      (amount, callback) => {
        callback(null);
        const toClaim = (totalBalance * amount) / totalAlgoProvided;
        // TODO: make it work with validator
        if (toClaim <= balance(wALGO)) {
          transfer([[toClaim, wALGO]]).to(this);
        }
        return [totalBalance];
      }
    );

  commit();

  // Loop forever to preserve the token
  Anybody.publish();
  var [] = [];
  invariant(true);
  while (true) {
    commit();
    Anybody.publish();
    continue;
  }

  commit();
});
