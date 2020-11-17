'reach 0.1';

class C {
  constructor(x) {
    this.x = x;
  }
}

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      const c = new C(x);
      exit();
    });
