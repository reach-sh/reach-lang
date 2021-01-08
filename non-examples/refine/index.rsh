'reach 0.1';

// These would be in stdlib or something
const Refined = (ty, predicate) => ({
  // ty is not used here, but would be used by the compiler:
  // CBR of Refined(ty, _) = CBR of ty
  // (Canonicalize additionally checks that predicate is satisfied)

  predicate,
  refine: (x) => {
    require(predicate(x));
    return x;
  },
});
const mkEnum = (n) => Refined(UInt, (x) => x < n);
// end stdlib

const E = mkEnum(3);

export const main = Reach.App(
  {},
  [['A', {
    getE: Fun([], UInt), // TODO: return E
  }], ['B', {
    showE: Fun([UInt], Null), // TODO: accept E
  }]],
  (A, B) => {
    A.only(() => {
      const eInt = declassify(interact.getE());
      assume(E.predicate(eInt));
      // TODO: assume would not be explicitly needed
      // if getHand returns Hand; it would be built in.
    });
    A.publish(eInt);
    // TODO: if the contract receives a publication of an E,
    // it should implicitly require E.predicate (?).
    // Either E.refine is called explicitly and adds a require to the next consensus block,
    // or `interact` returns an E in which case E.refine is called implicitly and adds a require to the next consensus block.
    // One way or another, consensus needs to certify E.predicate.
    const e = E.refine(eInt);
    commit();
    B.only(() => {
      interact.showE(e);
    });
  }
);
