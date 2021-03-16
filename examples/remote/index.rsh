'reach 0.1';

const CoolThing = {
  setX: Refine(Fun([UInt], Null), (([x]) => x > 2), ((_, _) => true)),
  getX: Fun([], Refine(UInt, (x => x > 2))),
};

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getAddr: Fun([], Address),
      getCT: Fun([], Tuple(UInt, Address)),
    }),
    Participant('Bob', {
      getX: Fun([], UInt),
      see: Fun([UInt, UInt, UInt, UInt], Null),
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
    const x0 = ctx.getX();
    const x1 = cty.getX.bill(amt / 2)();
    const [ amtr, x2 ] = cty.getX.withBill()();
    transfer(amt / 2).to(B);
    transfer(amtr).to(A);
    commit();

    B.only(() => {
      interact.see(x0, x1, amtr, x2); });
    exit();
  }
);
