'reach 0.1';

const MUInt = Maybe(UInt);

export const main = Reach.App(
  {},
  [['A', { get: Fun([], MUInt) }]],
  (A) => {
    A.only(() => {
      const mi = declassify(interact.get());
      const i = (() => {
        switch ( mi ) {
        case Some: return mi; } })();
    });
  } );
