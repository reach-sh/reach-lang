'reach 0.1';

class C {
  constructor(x) {
    this.x = x;
  }
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const c = new C(x);
      exit();
    });
