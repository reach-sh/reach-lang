'reach 0.1';

const obj = {};

export const main = Reach.App(
  {}, [], () => { return obj.x; }
);
