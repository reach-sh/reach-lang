'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const C = Participant('Creator', {
    firstHeir: Address,
    switchTime: UInt,
    ready: Fun([], Null),
  });
  const O = API('Owner', {
    ping: Fun([], Null),
    setNextHeir: Fun([Address], Null)
  });
  init();

  C.only(() => {
    const firstHeir = declassify(interact.firstHeir);
    const switchTime = declassify(interact.switchTime);
    assume(this != firstHeir, "firstHeir assume");
  });
  C.publish(firstHeir, switchTime);
  check(this != firstHeir, "firstHeir check");

  C.interact.ready();

  const [owner, heir] =
    parallelReduce([C, firstHeir])
    .invariant(balance() == 0)
    .invariant(owner != heir)
    .while(true)
    .api_(O.ping, () => {
      check(this == owner, "ping owner check");
      return [ (k) => {
        k(null);
        return [owner, heir];
      }];
    })
    .timeout(relativeTime(switchTime), () => {
      const [[nextHeir], k] =
        call(O.setNextHeir)
        .assume((nextHeir) => check(this == heir && this != nextHeir));
      check(this == heir && this != nextHeir, "setNextHeir new owner");
      k(null);
      return [heir, nextHeir];
    });

  commit();
  assert(false);
});
