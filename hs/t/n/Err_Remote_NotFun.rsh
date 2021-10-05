'reach 0.1';

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getCtc: Fun([], Contract),
    }),
  ],
  (A) => {
    A.only(() => {
      const ca = declassify(interact.getCtc()); });
    A.publish(ca);
    const ctx = remote(ca, { x: UInt });
    commit();

    exit();
  }
);
