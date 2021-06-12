'reach 0.1';

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getAddr: Fun([], Address),
    }),
  ],
  (A) => {
    A.only(() => {
      const ca = declassify(interact.getAddr()); });
    A.publish(ca);
    const ctx = remote(ca, { x: UInt });
    commit();

    exit();
  }
);
