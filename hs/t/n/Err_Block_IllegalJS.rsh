'reach 0.1';

const f = (x, o) => {
  with (o) {
    void x;
  }
}

export const main = Reach.App(
  {}, [], () => { return f(1, 2); }
);
