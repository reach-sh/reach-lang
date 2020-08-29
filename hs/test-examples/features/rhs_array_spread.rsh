'reach 0.1';

export const main = Reach.App(
  {},
  [['A', { get: Fun([],Tuple(UInt256, Bool, UInt256)),
           put: Fun([UInt256, Bool, UInt256], Null) }]],
  (A) => {
    A.only(() => {
      interact.put(...interact.get());
    });
  }
);
