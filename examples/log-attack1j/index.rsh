'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    get: Fun([], Object({
      addr: Address,
      x: UInt,
    })),
  });
  deploy();

  A.only(() => {
    const { addr, x } = declassify(interact.get()); });
  A.publish(addr, x);
  const ctc = remote(addr, {
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
