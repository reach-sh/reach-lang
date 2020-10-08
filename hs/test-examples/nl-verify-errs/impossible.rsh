'reach 0.1';

export const main = Reach.App(
  {}, [['A', {getX: Fun([], UInt)}]], (A) => {
    A.only(() => {
      const y = 0;
      const _x = interact.getX();
      possible(_x < y);
    });
  }
);
