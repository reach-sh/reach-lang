'reach 0.1';
'use strict';

const Posn = Struct([["x", UInt], ["y", UInt]]);
const PosnO = Object({x: UInt, y: UInt});
const PosnT = Tuple(UInt, UInt);

const CoolThing = {
  setX: Refine(Fun([Posn], Null), (([p]) => p.x > 2), ((_, _) => true)),
  getX: Fun([], Refine(Posn, (p => p.x > 2))),
  getXv: Fun([], Posn),
  incY: Fun([], Null),
};

const Common = {
  seeXv: Fun([Posn], Null),
};

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      ...Common,
      getAddr: Fun([], Address),
      getCT: Fun([], Tuple(UInt, Address)),
    }),
    Participant('Bob', {
      ...Common,
      getX: Fun([], UInt),
      see: Fun([UInt, UInt, UInt, UInt, UInt, Posn, PosnT, PosnO],
               Null),
    }),
  ],
  (A, B) => {
    A.only(() => {
      const ctxa = declassify(interact.getAddr());
      const cta = remote(ctxa, CoolThing);
      cta.incY();
      const [amt, ctya] = declassify(interact.getCT()); });
    A.publish(ctxa, amt, ctya).pay(amt);
    const ctx = remote(ctxa, CoolThing);
    const cty = remote(ctya, CoolThing);
    commit();

    B.only(() => {
      const bx = declassify(interact.getX());
      interact.seeXv(ctx.getXv());
      ctx.incY.pay(amt)();
      assume(bx > 2); });
    B.publish(bx);
    require(bx > 2);
    ctx.setX(Posn.fromObject({x: bx, y: 0}));
    cty.setX.pay(amt)(Posn.fromTuple([bx, 1]));
    commit();

    A.publish();
    const r0 = ctx.getX();
    const r1 = cty.getX.bill(amt / 2)();
    const [ amtr, r2 ] = cty.getX.withBill()();
    transfer(amt / 2).to(B);
    transfer(amtr).to(A);
    commit();

    A.only(() => {
      interact.seeXv(cty.getXv()); });
    B.only(() => {
      interact.see(r0.x, r0.y, r1[0], r1[1], amtr,
        r0, Struct.toTuple(r1), Posn.toObject(r2)); });
    exit();
  }
);
