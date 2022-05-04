'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.only(() => {
        assert(sqrtApprox(9, 5) == 3, "sqrt 9 = 3");
        assert(sqrtApprox(10, 5) == 3, "sqrt 10 = 3");
        assert(sqrtApprox(1024, 5) == 32, "sqrt 1024 = 32");
        assert(sqrtApprox(336400, 10) == 580, "sqrt 336400 = 580");
      });
    }
  );
