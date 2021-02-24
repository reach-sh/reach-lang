'reach 0.1';

export const main = Reach.App(
  {},
  [Participant("A", {getKey: Fun([], Bytes(4))})],
  (A) => {
    A.only(() => {
      const key = interact.getKey();
      const obj = {[key]: 1}
    });
  }
);
