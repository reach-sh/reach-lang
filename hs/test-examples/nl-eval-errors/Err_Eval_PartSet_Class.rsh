'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {
      getAddr: Fun([], Address),
      }),
      ParticipantClass('C', {}) ],
    (A, C) => {
      A.only(() => {
        const addr = declassify(interact.getAddr()); });
      A.publish(addr);
      C.set(addr);
      commit();
      exit();
    });
