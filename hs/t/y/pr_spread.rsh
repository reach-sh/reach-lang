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
      () => ({ msg: 1, when: declassify(interact.go()) }),
      (...args) => {
        const [ y ] = args;
        A.interact.ok();
        return x + y;
      })
    .timeout(false);

  commit();

})

