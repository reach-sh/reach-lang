'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {
      showBool: Fun([Bool], Null),
      showInt: Fun([UInt], Null),
    })],
    (A) => {
      const e = Either(UInt, Bool);
      A.only(() => {
        const l = e.Left(5);
        const r = e.Right(false);
        either(l, interact.showInt, interact.showBool);
        either(r, interact.showInt, interact.showBool);
        assert(isLeft(l));
        assert(!isLeft(r));
        assert(isRight(r));
        assert(!isRight(l));
        assert(fromLeft(l, 0) == 5);
        assert(fromLeft(r, 0) == 0);
        assert(fromRight(r, true) == false);
        assert(fromRight(l, true));
      });
    });

