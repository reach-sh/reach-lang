'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {getWager: Fun([],UInt256)}],],
  (A) => {
    A.only(() => {
      declassify(interact.wager);
    });
  }
);
