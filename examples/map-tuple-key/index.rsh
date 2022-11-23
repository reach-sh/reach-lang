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
  invariant(m.size() == 0);
  invariant(balance() == 0);
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

    commit();
    A.publish();
    delete m[k];

    i = i + 1;
    continue;
  }

  commit();
});
