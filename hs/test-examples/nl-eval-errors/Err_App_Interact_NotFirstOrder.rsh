'reach 0.1';

export const main = Reach.App(
  {}, [Participant("A", {f: Fun([], Fun([], UInt))})], (A) => {}
);
