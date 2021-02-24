'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    // false should instead be a "call-like" expression
    unknowable(A, false);
  }
);
