'reach 0.1';

const f = (y+1) => y;

export const main = Reach.App(
  {}, [], () => { return f(0); }
);
