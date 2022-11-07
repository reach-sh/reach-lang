'reach 0.1';


const minBal = 100000;

const ReachCtcInterface = {
  'publish': Fun([UInt, UInt, UInt, Tuple(UInt, UInt)], Null),
}

export const calcApp = Reach.App(() => {
  const A = Participant('Alice', {});
  const B = API({ add1: Fun([UInt], UInt) });
  init();

  A.publish();
  commit();

  const [ [i], k ] = call(B.add1);
  k(i + 1);
  commit();
});

export const main = Reach.App(() => {
  const reachConstructor = Struct([ ["time", UInt256], ["msg", Bool] ]);
  const A = Participant('Alice', {
    x: UInt,
    amt: UInt,
    isALGO: Bool,
    chk: Fun(true, Null),
  });
  const calcInterface = {
    ...ReachCtcInterface,
    'add1': Fun([UInt], UInt)
  };
  init();

  A.only(() => {
    const isALGO = declassify(interact.isALGO);
    const x = declassify(interact.x);
    const amt = declassify(interact.amt); });
  A.publish(amt, isALGO, x)
    .pay(amt)
    .check(() => { check(amt > minBal); });
  commit();

  A.publish();
  const ctc = (new Contract(calcApp))([0]);
  const { publish, add1 } = remote(ctc, calcInterface);
  commit();

  A.publish();
  if (isALGO) {
    publish
      .pay(minBal)
      .ALGO({ rawCall: true })
      (0, 0, 0, [255, 1]);
  }
  commit();

  A.publish();
  const r = add1
            .bill(isALGO ? minBal : 0)
            .ALGO({
              onCompletion: 'DeleteApplication',
              simReturnVal: x + 1,
            })(x);
  enforce(r == x + 1);

  commit();

  A.publish();
  transfer(balance()).to(A);
  A.interact.chk(r);
  commit();
});

export const xtra = main;
