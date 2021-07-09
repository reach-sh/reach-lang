'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    const {x() { return 1 }} = {x: 2};
  }
);
