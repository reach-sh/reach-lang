'reach 0.1';

const f = () => { return 3 };

export const main = Reach.App(
  {}, [], () => { return f(5); }
);
