'reach 0.1';

const Remote = {
  payFn: Fun([], Null),
  bilFn: Fun([], Null),
};

export const main = Reach.App(
  { connectors: [ETH] },
  [
    Participant('Alice', {
      token: Token,
      getCt: Fun([], Tuple(UInt, Address))
    }),
    Participant('Bob', {})
  ],
  (Alice, Bob) => {
    Alice.only(() => {
      const token = declassify(interact.token);
      const [amt, cta] = declassify(interact.getCt());
    });
    Alice.publish(token, amt, cta)
      .pay([ [amt, token] ]);

    // Send money to contract
    const ctc = remote(cta, Remote);
    ctc.payFn.pay([ [amt, token] ])();

    commit();

    Bob.publish();

    ctc.bilFn.bill(amt)();

    // Bob will look at contract to receive money
    transfer(balance()).to(Bob);
    commit();

  }
);
