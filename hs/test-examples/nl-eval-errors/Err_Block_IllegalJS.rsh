'reach 0.1';

const f = () => {
  const obj = {x: 0};
  with(obj) { return x; };
}

export const main = Reach.App(
  {}, [], () => { return f(); }
);
