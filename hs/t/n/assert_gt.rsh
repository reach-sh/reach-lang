'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {getX: Fun([], UInt)})], (A) => {
    A.only(() => {
      const _x = interact.getX();
      const y = 0;
      assert(_x > y);
    });
  }
);
