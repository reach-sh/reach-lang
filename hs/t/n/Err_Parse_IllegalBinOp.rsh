'reach 0.1';

const b = "k" in {"k": 5}; 

export const main = Reach.App(
  {}, [], () => { return b; }
);
