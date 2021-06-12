'reach 0.1';

export const main = Reach.App(
  {},
  [Participant('A', { get: Fun([],Tuple(UInt, Bool, UInt)),
           put: Fun([UInt, Bool, UInt], Null) })],
  (A) => {
    A.only(() => {
      interact.put(...interact.get());
    });
  }
);
