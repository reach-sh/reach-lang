'reach 0.1';

const CoolThing = {
  setX: Refine(Fun([UInt], Null), (([x]) => x > 2), ((_, _) => true)),
  getX: Fun([], Struct([ ["x", Refine(UInt, (x => x > 2))],
                         ["y", UInt],
        ])),
};

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getAddr: Fun([], Address),
      getCT: Fun([], Tuple(UInt, Address)),
    }),
    Participant('Bob', {
      getX: Fun([], UInt),
      see: Fun([UInt, UInt, UInt, UInt, UInt,
                Struct([["x", UInt], ["y", UInt]])],
               Null),
    }),
  ],
  (A, B) => {
    A.only(() => {
      const ctxa = declassify(interact.getAddr());
      const [amt, ctya] = declassify(interact.getCT()); });
    A.publish(ctxa, amt, ctya).pay(amt);
    const ctx = remote(ctxa, CoolThing);
    const cty = remote(ctya, CoolThing);
    commit();

    B.only(() => {
      const bx = declassify(interact.getX());
      assume(bx > 2); });
    B.publish(bx);
    require(bx > 2);
    ctx.setX(bx);
    cty.setX.pay(amt)(bx);
    commit();

    A.publish();
    const r0 = ctx.getX();
    const r1 = cty.getX.bill(amt / 2)();
    const [ amtr, r2 ] = cty.getX.withBill()();
    transfer(amt / 2).to(B);
    transfer(amtr).to(A);
    commit();

    B.only(() => {
      interact.see(r0.x, r0.y, r1[0], r1[1], amtr, r2); });
    exit();
  }
);
