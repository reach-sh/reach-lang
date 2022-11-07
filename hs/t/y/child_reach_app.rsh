'reach 0.1';

export const app = (y) => Reach.App(() => {
  const B = Participant('A', { x: UInt });
  init();
  B.only(() => {
    const x = declassify(interact.x);
  });
  B.publish(x);
  enforce(x == y);
  commit();
})

export const t = app(5);

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

  const ctor1 = new Contract(t);

  const ctor2 = new Contract(Reach.App(() => {
    const B = Participant('A', {
      ...hasConsoleLogger,
      d: UInt
    });
    init();

    B.only(() => {
      const d = declassify(interact.d);
    });
    B.publish(d);
    // Can access stdlib functions
    const e = add(b, d);
    assert(balance() == 0);
    transfer(balance()).to(B);
    assert(e == 5 + d);
    commit();
  }));

  const ctor3 = new Contract(app(0));

  commit();
});
