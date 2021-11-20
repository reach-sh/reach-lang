'reach 0.1';

import { MUInt, MA, MBS } from './type.rsh';

// Based on examples/view-fun
export const fun = (A, v, l) => {
  const f = (a, b) => A.interact.checkViewFun(l, a, b);

  f(0, MUInt.None());
  f(1, MUInt.None());
  f(2, MUInt.None());

  A.publish();
  v.fun.set(x => x + 1);
  f(0, MUInt.Some(1));
  f(1, MUInt.Some(2));
  f(2, MUInt.Some(3));
  commit();

  A.publish();
  v.fun.set(x => x - 1);
  f(0, MUInt.None());
  f(1, MUInt.Some(0));
  f(2, MUInt.Some(1));
  commit();

  A.publish();
  v.fun.set();
  f(0, MUInt.None());
  f(1, MUInt.None());
  f(2, MUInt.None());
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
