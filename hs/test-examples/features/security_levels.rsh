'reach 0.1';

// This function requires that its arg is secret,
// and returns a secret.
const fnSecret = (_x) => { return 10 * _x; };

// This function requires that its arg is secret,
// but returns the declassified version.
const fnDeclassify = (_x) => { return declassify(_x); };

// This function accepts either a secret or public arg,
// and returns w/ the same secrecy
const fnPolymorphic = (x) => { return x + 1; };

export const main = Reach.App(
  {}, [['A', {x: UInt256}]], (A) => {
    A.only(() => {
      const _x = interact.x;
      assume(_x == 1);

      const _x10 = fnSecret(_x);
      require(_x10 == 10);

      const x = fnDeclassify(_x);
      require(x == 1);

      const x2 = fnPolymorphic(x);
      require(x2 == 2);

      const _x11 = fnPolymorphic(_x10);
      require(_x11 == 11);
    });
  }
);
