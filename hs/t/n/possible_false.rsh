'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    possible(false);
    possible(false); // Multiple failures display
  }
);
