'reach 0.1';

export const main = Reach.App(
  {},
  [["A", {getKey: Fun([], Bytes)}]],
  (A) => {
    A.only(() => {
      const key = interact.getKey();
      const obj = {[key]: 1}
    });
  }
);
