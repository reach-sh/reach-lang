'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    A.only(() => {
      const x = declassify(0);
    }, 2);
    A.publish(x);
    return x;
  }
);
