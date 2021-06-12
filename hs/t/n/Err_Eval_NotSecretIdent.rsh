'reach 0.1';

export const main = Reach.App(
  {}, [Participant("A", {x: UInt})], (A) => {
    A.only(() => {
      const x = interact.x;
    });
  }
);
