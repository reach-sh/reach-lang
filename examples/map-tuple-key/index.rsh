'reach 0.1';

export const main = Reach.App(() => {
  const KEY_TYPE = Tuple(UInt, Bool);
  const VAL_TYPE = UInt;

  const A = Participant('A', {
    getKey: Fun([], KEY_TYPE),
    getValue: Fun([], UInt),
    lookup: Fun([], KEY_TYPE),
    chk: Fun(true, Null),
  });

  init();

  A.publish();

  const m = new Map(KEY_TYPE, VAL_TYPE);

  var i = 0;
  invariant(true);
  while (i < 5) {
    commit();

    A.only(() => {
      const k = declassify(interact.getKey());
      const v = declassify(interact.getValue());
      const l = declassify(interact.lookup());
    });
    A.publish(k, v, l);

    m[k] = v;

    A.interact.chk(m[l]);

    i = i + 1;
    continue;
  }

  transfer(balance()).to(A);
  commit();

});
