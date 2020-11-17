'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      const {a: { b = 5 }} = { a: {} }; // b will equal 5 if not present in a's object.
    });
