'reach 0.1';


export const main =
  Reach.App(
    {},
    [['A', { show: Fun([FixedPoint], Null) }]],
    (A) => {
      A.only(() => {
        interact.show( fxsqrt( fx(1)( int(Neg, 9) ) , 10 ) );
      });
    });
