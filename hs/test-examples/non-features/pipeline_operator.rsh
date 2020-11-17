'reach 0.1';
// Stage 1 JS feature, pipe LHS into RHS function.
// Docs:
//    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Pipeline_operator

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      const double = x => x * 2;
      5 |> double |> Array.iota;
    });
