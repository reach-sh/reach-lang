'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    A.publish().timeout(0, 0);
    return 0;
  }
);
