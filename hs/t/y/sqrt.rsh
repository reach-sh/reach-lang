'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.only(() => {
        assert(sqrt(9, 5) == 3, "sqrt 9 = 3");
        assert(sqrt(10, 5) == 3, "sqrt 10 = 3");
        assert(sqrt(1024, 5) == 32, "sqrt 1024 = 32");
        assert(sqrt(336400, 10) == 580, "sqrt 336400 = 580");
      });
    }
  );
