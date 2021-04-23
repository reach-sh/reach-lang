'reach 0.1';

const Posn = Struct([["x", UInt], ["y", UInt]]);
const PosnO = Object({x: UInt, y: UInt});
const PosnT = Tuple(UInt, UInt);

const CoolThing = {
  setX: Refine(Fun([Posn], Null), (([p]) => p.x > 2), ((_, _) => true)),
  getX: Fun([], Struct([ ["x", Refine(UInt, (x => x > 2))],
                         ["y", UInt],
        ])),
  payFn: Fun([Token], Null),
  bilFn: Fun([Token], Null),
};

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getAddr: Fun([], Address),
      getCT: Fun([], Tuple(UInt, Address)),
      getGIL: Fun([], Token),
      getZMD: Fun([], Token),
    }),
    Participant('Bob', {
      getX: Fun([], UInt),
      see: Fun([UInt, UInt, UInt, UInt, UInt, Posn, PosnT, PosnO],
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
    ctx.setX(Posn.fromObject({x: bx, y: 0}));
    cty.setX.pay(amt)(Posn.fromTuple([bx, 1]));
    commit();

    A.publish();
    const r0 = ctx.getX();
    const r1 = cty.getX.bill(amt / 2)();
    const [ amtr, _, r2 ] = cty.getX.withBill()();
    transfer(amt / 2).to(B);
    transfer(amtr).to(A);
    commit();


    // Test transferring non-network tokens to and from a remote ctc

    A.only(() => {
      const gil = declassify(interact.getGIL());
      const zmd = declassify(interact.getZMD());
      assume(gil != zmd); });

    A.publish(gil, zmd)
     .pay([ [amt * 2, gil], [amt, zmd] ]); // CTC has 2x gil, x zmd
    commit();

    A.publish();
    ctx.payFn.pay([ [amt, gil] ])(gil);   // CTC has x gil, x zmd
    commit();

    B.publish();
    ctx.bilFn.bill([ [amt, gil] ])(gil);  // CTC has 2x gil, x zmd
    transfer(amt, gil).to(B);             // CTC has x gil, x zmd
    commit();

    A.publish();
    ctx.payFn.pay([ [amt, gil] ])(gil);   // CTC has 0 gil, x zmd
    commit();

    B.publish();
    const [ netRecv, [ gilRecv ], _ ] = ctx.bilFn.withBill([ gil ])(gil);
    transfer(netRecv).to(B);      // CTC has x gil, x zmd
    transfer(gilRecv, gil).to(B); // CTC has 0 gil, x zmd
    transfer(amt, zmd).to(B);     // CTC has 0 gil, 0 zmd


    commit();

    B.only(() => {
      interact.see(r0.x, r0.y, r1[0], r1[1], amtr,
        r0, Struct.toTuple(r1), Posn.toObject(r2)); });
    exit();
  }
);
