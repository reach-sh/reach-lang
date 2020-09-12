'reach 0.1';

const MUInt256 = Maybe(UInt256);

export const main = Reach.App(
  {},
  [['A', { get: Fun([], MUInt256) }]],
  (A) => {
    A.only(() => {
      const mi = declassify(interact.get());
      const i = (() => {
        switch ( mi ) {
        case None: return 0;
        case Mone: return 1;
        case Some: return mi; } })();
    });
  } );
