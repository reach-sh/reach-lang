'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    const obj = 0;
    return obj.y;
  }
);
