'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    A.only(() => {
      // This added failure is just to make the test suite pass.
      assert(false);

      // XXX the below assume should cause a failure on its own.
      // This is a known failing test case.
      assume(false);
    });
  }
);
