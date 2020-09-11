'reach 0.1';

const f = () => {
  const obj = {x: 0};
  throw obj;
}

export const main = Reach.App(
  {}, [], () => { return f(); }
);
