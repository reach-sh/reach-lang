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

  const x = parallelReduce(0)
    .while(x < 5)
    .invariant(balance() == 0)
    .case(A,
      (() => ({ when: declassify(interact.go()) })),
      ((_) => x + 1))
    .timeout(false);

  commit();
});
