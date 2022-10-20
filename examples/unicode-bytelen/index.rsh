'reach 0.1';

export const main = Reach.App(() => {
  const I = { s: Tuple(Bytes(8), Bytes(8)) };
  const D = Participant('D', { ...I, ready: Fun([], Null) });
  const V = View({ ...I });
  const A = API({halt: Fun([], Null)});
  init();
  D.only(() => {
    const s = declassify(interact.s);
  });
  D.publish(s);
  V.s.set(s);
  commit();
  D.interact.ready();
  const [[], k] = call(A.halt)
    .check(() => { check(this == D); });
  k(null);
  commit();
});
