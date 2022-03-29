'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const C = Participant('Creator', {
    firstHeir: Address,
    ready: Fun([], Null),
  });
  const O = API('Owner', {
    ping: Fun([], Null),
    setNextHeir: Fun([Address], Null)
  });
  init();

  C.only(() => {
    const firstHeir = declassify(interact.firstHeir);
    assume(this != firstHeir);
  });
  C.publish(firstHeir);
  check(this != firstHeir);

  C.interact.ready();

  const [owner, heir] =
    parallelReduce([C, firstHeir])
    .invariant(balance() == 0)
    .while(true)
    .api(O.ping,
      () => check(this == owner),
      () => 0,
      (k) => {
        check(this == owner);
        k(null);
        return [owner, heir];
      }
    )
    .timeout(relativeTime(3), () => {
      const [[nextHeir], k] =
        call(O.setNextHeir)
        .assume((nextHeir) => check(this != nextHeir));
      check(this != nextHeir);
      k(null);
      return [heir, nextHeir];
    });

  commit();
  assert(false);
});
