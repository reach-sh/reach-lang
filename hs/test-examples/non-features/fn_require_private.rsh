'reach 0.1';

// Naming this arg _x means that the compiler will enforce
// that it is private, not public.
export const f = (_x) => {
  return _x;
};

export const main = Reach.App(
  {}, [], () => {
    // TODO: the error message should point to the call site
    const x = f(1);
  }
);
