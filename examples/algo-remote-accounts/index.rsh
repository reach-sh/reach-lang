'reach 0.1';

export const client = Reach.App(() => {
  const A = Participant('Alice', {
    setup: Tuple(Address, UInt, Contract),
  });
  init();

  A.only(() => {
    const [payTo, amount, rctc] = declassify(interact.setup);
  })
  A.publish(payTo, amount, rctc).pay(amount);

  const r = remote(rctc, {
    doPay: Fun([UInt], Null),
  });

  r.doPay.pay(amount)(amount);
  commit();

  exit();
});

export const server = Reach.App(() => {
  const A = Participant('Alice', {
    setup: Tuple(Address),
    ready: Fun([Contract], Null),
  });

  const I = API({
    doPay: Fun([UInt], Null)
  });

  init();

  A.only(() => {
    const [payTo] = declassify(interact.setup);
  })
  A.publish(payTo);
  A.interact.ready(getContract());
  commit();

  const [[amount], k] = call(I.doPay).pay((amount) => amount);
  transfer(amount).to(payTo);
  k(null);
  commit();

  exit();
});
