'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {getWager: Fun([],UInt)}]],
  (A) => {
    A.only(() => {
      declassify(interact.wager);
    });
  }
);
