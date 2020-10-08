'reach 0.1';

export const main = Reach.App(
  {},
  [['Alice', {x: UInt}]],
  (A) => {
    A.only(() => {
      const x = declassify(interact.x);
    });
    A.publish(x);

    var [i, mx0] = [0, Maybe(UInt).None()];
    invariant(true);
    while (i < x) {
      const nextI = i + 1;
      [i, mx0] = [nextI, Maybe(UInt).Some(nextI)];
      continue;
    }
    commit();
  }
);

// TODO: fixed for UInt and Null, but not for all types
// Error: Invalid type for argument in function call.
// Invalid implicit conversion from int_const 0 to bool requested.
//   --> ./build/data.main.sol:53:46:
//   |
//   53 |       l2(a2(_a.v1, _f.v10, T0(_enum_T0.Some, 0, _f.v10))); }
