'reach 0.1';

export const Interface = {
  collect: Fun([], Bool)
}

const sharedParams = { x: UInt, y: UInt, tok: Token }

export const server = Reach.App(() => {
  const D = Participant('Deployer', {
    params: Object(sharedParams),
    onDeployed: Fun(true, Null),
  });
  const C = API(Interface);
  init();

  D.only(() => {
    const { x, y, tok } = declassify(interact.params);
  });
  D.publish(x, y, tok);
  D.interact.onDeployed();
  commit();

  const [ _, k ] = call(C.collect)
                    .pay(() => [ y, [x, tok] ]);
  k(true);
  commit();

  D.publish();
  transfer(y).to(D);
  transfer(x, tok).to(D);
  commit();
});

export const client = Reach.App(() => {
  const A = Participant('A', {
    params: Object({ ...sharedParams, ctc: Contract }),
  });
  init();
  A.only(() => {
    const { ctc, x, y, tok } = declassify(interact.params);
  });
  A.publish(ctc, x, y, tok);
  commit();
  A.pay([ 0, [x, tok] ]);
  const _ = remote(ctc, Interface)
              .collect
              .ALGO({ strictPay: true })
              .pay([ 0, [x, tok] ])();
  commit();
});
