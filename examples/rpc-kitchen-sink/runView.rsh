'reach 0.1';

const MBool = Maybe(Bool);
const BS    = Bytes(96);
const MA    = Maybe(Address);
const MBS   = Maybe(BS);

export const T = {
  fun:  Fun([UInt, UInt], Bool),
  who:  Address,
  meta: BS,
};

export const pii = {
  checkViewFun:   Fun([ Bytes(10), UInt, UInt, MBool ], Null),
  checkViewBytes: Fun([ Bytes(10), Bytes(10), Tuple(MA, MBS) ], Null),
  meta:           BS,
};

export const fun = (A, v, l) => {
  const f = (a, b, c) => A.interact.checkViewFun(l, a, b, c);

  f(0, 0, MBool.None());
  f(2, 0, MBool.None());
  f(0, 3, MBool.None());

  A.publish();
  v.fun.set((x, y) => x === y);
  f(0, 0, MBool.Some(true));
  f(2, 0, MBool.Some(false));
  f(0, 3, MBool.Some(false));
  commit();

  A.publish();
  v.fun.set((x, y) => x !== y);
  f(0, 0, MBool.Some(false));
  f(2, 0, MBool.Some(true));
  f(0, 3, MBool.Some(true));
  commit();

  A.publish();
  v.fun.set();
  f(0, 0, MBool.None());
  f(2, 0, MBool.None());
  f(0, 3, MBool.None());
  commit();
};

// Based on examples/view-bytes
export const vbytes = (A, v, l1, l2) => {
  const f = (a, b) => A.interact.checkViewBytes(l1, l2, [ a, b ]);

  f(MA.None(), MBS.None());

  A.only(() => { const meta = declassify(interact.meta); });
  A.publish(meta);
  v.who.set(A);
  v.meta.set(meta);
  commit();
  f(MA.Some(A), MBS.Some(meta));

  A.publish();
  v.who.set();
  commit();
  f(MA.None(), MBS.Some(meta));

  A.publish();
  v.who.set(A);
  v.meta.set();
  commit();
  f(MA.Some(A), MBS.None());

  A.publish();
  v.who.set();
  f(MA.None(), MBS.None());
  commit();

  A.publish();
  v.meta.set();
  commit();
  f(MA.None(), MBS.None());
};
