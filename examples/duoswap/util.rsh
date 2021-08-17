'reach 0.1';

export const NUM_OF_TOKENS = 2;

export const avg = (a, b) => (a + b) / 2;

export const getAmtOut = (amtIn, reserveIn, reserveOut) => {
  const amtInWithFee = amtIn * 997;
  const num = amtInWithFee * reserveOut;
  const den = (reserveIn * 1000) + amtInWithFee;
  return num / den;
}

// Calculates how many LP tokens to mint
export const mint = (amtIn, bal, poolMinted) => {
  const t = amtIn * poolMinted;
  assume(t >= bal, "t >= bal");
  return t / ((bal == 0) ? 1 : bal);
};

// Types
export const MkArray = (ty) => Array(ty, NUM_OF_TOKENS);

export const MToken = Maybe(Token);

export const Market = Object({
  k: UInt
});

export const State = Tuple(Bool, Market);

export const Withdraw = Object({
  liquidity: UInt
});

export const Deposit = Object({
  amtA: UInt,
  amtB: UInt
});

export const Trade = Object({
  amtA: UInt,
  amtB: UInt,
  amtInTok: Maybe(Token),
});

// Participants
export const ProviderInterface = {
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
  acceptToken: Fun([Token], Null),
};

export const mkAdminInterface = (hasTokA) => {
  const base = {
    tokB: Token,
    conUnit: UInt,
    shouldClosePool: Fun([State], Object({
      when: Bool,
      msg : Null,
    }))
  };
  return hasTokA ? { ...base, tokA: Token} : base;
};

export const TraderInterface = {
  ...hasConsoleLogger,
  logMarket: Fun(true, Null),
  tradeMaybe: Fun([State], Object({
    when: Bool,
    msg : Trade,
  })),
  tradeDone: Fun([Bool, Tuple(UInt, MToken, UInt, MToken)], Null),
  acceptToken: Fun([Token], Null),
};

export const TokensView = {
  aTok : Token,
  bTok : Token,
  aBal : UInt,
  bBal : UInt,
};
