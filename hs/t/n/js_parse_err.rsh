
'reach 0.1';

export const main = Reach.App(
  {},
  [Participant('A', {}), Participant('B', {})],
  (A   B) => {
    // ^ 7:8 the B ident is unexpected,
    // there should be a comma before it.
    exit();
  }
);
