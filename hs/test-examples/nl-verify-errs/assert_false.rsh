'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    assert(false);
    assert(false); // Multiple failures display
  }
);
