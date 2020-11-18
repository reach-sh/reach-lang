'reach 0.1';

export const main = Reach.App(
  {}, [['A', {x: UInt}]], (A) => {
    A.only(() => {
      const _x = interact.x;
    })
    // false should instead be a participant
    unknowable(false, A(_x));
  }
);
