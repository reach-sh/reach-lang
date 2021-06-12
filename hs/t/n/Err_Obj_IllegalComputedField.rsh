'reach 0.1';

const x = {[ 2+2 ]: "four"}

export const main = Reach.App(
  {}, [], () => {return x[4];}
);
