'reach 0.1';

const x = 1;

export const main = Reach.App(
  {}, [], () => { return ++x; }
);
