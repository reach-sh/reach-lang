'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    a: UInt
  });
  init();

  A.only(() => {
    const a = declassify(interact.a);
  });
  A.publish(a);

  // This should be accessible in `ctor2`
  const b = 5;
  // This should not be because it is not statically known
  const c = a + b;

  const ctor = new Contract(Reach.App(() => {
    const B = Participant('A', {
      ...hasConsoleLogger,
      d: UInt
    });
    init();
    assert(c == a + b);
  }));

  commit();
});
