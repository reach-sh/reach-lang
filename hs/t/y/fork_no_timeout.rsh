'reach 0.1';

export const main = Reach.App(() => {
  const C = Participant('Constructor', {});
  const A = ParticipantClass('A', { go: Fun([], Bool), ok: Fun([], Null) });
  deploy();

  C.publish();
  commit();

  fork()
    .case(A,
      () => ({ when: declassify(interact.go()) }),
      () => {
        A.interact.ok();
      })
    .timeout(false);

  commit();

})