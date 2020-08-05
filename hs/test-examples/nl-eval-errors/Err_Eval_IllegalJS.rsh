'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    yield 1;
    return 0;
  }
);
