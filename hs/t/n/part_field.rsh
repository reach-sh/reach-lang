'reach 0.1';

export const main = Reach.App(
  {},
  [Participant('A', {getWager: Fun([],UInt)})],
  (A) => {
    A.only(() => {
      declassify(interact.wager);
    });
  }
);
