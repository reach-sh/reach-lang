'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    // underscore is the only binding
    // that is allowed to be bound multiple times

    // Ignore parts of an array
    const [_, x, _] = [1, 2, 3];

    // Ignore arguments
    const f = (_, _, z) => { return z; };
    const v = f('blah', null, x);
  }
);
