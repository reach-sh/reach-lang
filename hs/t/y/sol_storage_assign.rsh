'reach 0.1';

export const f = (ty) => Reach.App(() => {
  const A = Participant('A', {
    get: Fun([], ty),
    log: Fun(true, Null),
  });
  init();

  A.publish();

  const map = new Map(ty);

  commit();

  A.only(() => {
    const x = declassify(interact.get());
  });
  A.publish(x);
  map[A] = x;
  commit();

  A.publish();
  A.interact.log(map[A]);
  commit();

});

export const test1 = f(StringDyn);
export const test2 = f(BytesDyn);
export const test3 = f(Bytes(64));
export const test4 = f(Bytes(4));
export const test5 = f(Array(UInt, 5));
export const test6 = f(Array(Tuple(UInt, Bool), 5));
export const test7 = f(Array(Tuple(Array(UInt, 5), Bool), 5));
export const test8 = f(Object({ x: UInt, y: Bool }));
export const test9 = f(Object({ x: UInt, y: Array(Array(UInt, 4), 5) }));
export const test10 = f(Struct([["x", UInt], ["y", UInt]]));
export const test11 = f(Struct([["x", UInt], ["y", Array(Array(Array(UInt, 4), 4), 5)]]));
export const test12 = f(Object({ x: Array(Bytes(64), 10), y: Bool }));
