
'reach 0.1';

export const main = Reach.App(
  {},
  [['A', {}], ['B', {}]],
  (A   B) => {
    // ^ 7:8 the B ident is unexpected,
    // there should be a comma before it.
    exit();
  }
);
