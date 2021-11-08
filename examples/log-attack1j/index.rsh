'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    get: Fun([], Object({
      ctcInfo: Contract,
      x: UInt,
    })),
  });
  deploy();

  A.only(() => {
    const { ctcInfo, x } = declassify(interact.get()); });
  A.publish(ctcInfo, x);
  const ctc = remote(ctcInfo, {
    f: Fun([UInt], Null),
  });
  ctc.f(x);
  commit();

  A.publish();
  ctc.f(x);
  commit();

  exit();
}
);
