'reach 0.1';

export const main = Reach.App(() => {
  const C = Participant('Constructor', { });
  const A = ParticipantClass('A', {
    go: Fun([], Bool),
    ok: Fun([], Null) });
  deploy();

  C.publish();
  commit();

  A.publish();

  var x = 0;
  invariant(balance() == 0);
  while (x < 5) {
    commit();
    A.only(() => {
      const t32 = (() => ({ when: declassify(interact.go()) }))();
      const fork32_when = t32.when;
      const fork32_msg = null;
    });
    A.publish(fork32_msg)
     .when(fork32_when)
     .pay(((_) => 0)(fork32_msg))
     .timeout(false);
    const pr25_res = (() => x + 1)();
    x = pr25_res;
    continue;
  }
  commit();
});
