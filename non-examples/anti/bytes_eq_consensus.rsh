'reach 0.1';

export const main = Reach.App(
  {}, [['A', {x: Bytes}]], (A) => {
    A.only(() => {
      const x = declassify(interact.x);
      assume(bytes_eq(x, 'x'));
    });
    A.publish(x);
    require(bytes_eq(x, 'x'));
    // Error: Operator == not compatible with types bytes calldata and literal_string "x"
    // --> build/bytes_eq_consensus.main.sol:33:14:
    //   |
    //   33 |     require((_a.v2 == "x"));
    //   |              ^^^^^^^^^^^^

  }
);
