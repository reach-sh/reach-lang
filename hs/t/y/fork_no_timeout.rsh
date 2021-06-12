'reach 0.1';

export const main = Reach.App(() => {
  const A = ParticipantClass('A', { go: Fun([], Bool), ok: Fun([], Null) });
  deploy();


  fork()
    .case(A,
      (() => ({ when: declassify(interact.go()) })),
      (() => {
        A.interact.ok();
      }))
    .timeout(false);

  commit();

})