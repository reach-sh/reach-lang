'reach 0.1';
// Assigns object property to RHS if property is `undefined | nullish`.
// Relies on object mutation.
// Docs:
//    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_nullish_assignment

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      const obj = { a: 1 };
      obj.a ??= 100;
      obj.b ??= 200;
      // Expected:
      //    obj = { a: 1, b: 200 }
    });
