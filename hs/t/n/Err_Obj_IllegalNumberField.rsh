'reach 0.1';

const x = {4: "four"}

export const main = Reach.App(
  {}, [], () => {return x[4];}
);
