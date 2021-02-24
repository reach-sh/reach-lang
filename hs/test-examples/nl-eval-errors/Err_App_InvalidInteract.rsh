'reach 0.1';

export const main = Reach.App(
  {}, [Participant("A", {getHand: 3})], (A) => { return 0; }
);
