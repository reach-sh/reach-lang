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
      getTok: Fun([], Token),
      checkBal: Fun([], Null),
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
    // Todo make no args = []
    const [ amtr, _, r2 ] = cty.getX.withBill([])();
    transfer(amt / 2).to(B);
    transfer(amtr).to(A);
    commit();

    A.only(() => {
      const tok = declassify(interact.getTok());
      interact.checkBal(); });
    A.publish(tok).pay([ [amt, tok] ]);
    commit();

    // The balance is in contract!
    A.only(() => {
      interact.checkBal(); });
    A.publish();

    ctx.payFn.pay([ [amt, tok] ])(tok);
    commit();

    B.publish();

    // ctx.bilFn.bill([ [amt, tok] ])(tok);
    // transfer(amt, tok).to(B);

    // [ UInt, Array (N, [UInt, Token] ), returnValue ]
    const [ netTokAmt, [ nonNetTokAmt ], _ ] = ctx.bilFn.withBill([ tok ])(tok);
    transfer(netTokAmt).to(B);
    transfer(nonNetTokAmt, tok).to(B);

    commit();

    B.only(() => {
      interact.see(r0.x, r0.y, r1[0], r1[1], amtr,
        r0, Struct.toTuple(r1), Posn.toObject(r2)); });
    exit();
  }
);
