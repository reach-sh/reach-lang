'reach 0.1';

const x = {f() { return 0; }};

export const main = Reach.App(
  {}, [], () => {return x.f();}
);
