'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ verifyArithmetic: true });
  const A = Participant('A', {
    get: Fun([], UInt),
    put: Fun([UInt], Null),
  });
  deploy();

  A.only(() => {
    const x = declassify(interact.get());
    assume(x < UInt.max / 2, "manual mul check");
  });
  A.publish(x);
  require(x < UInt.max / 2, "manual mul check");
  const y = x * 2;
  commit();

  A.only(() => {
    interact.put(y);
  });

  exit();
});
