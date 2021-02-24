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
  {}, [Participant('A', {x: UInt})], (A) => {
    A.only(() => {
      const _x = interact.x;
      assume(_x == 1);

      const _x10 = fnSecret(_x);
      const x10 = declassify(_x10);

      const x = fnDeclassify(_x);

      const x2 = fnPolymorphic(x);

      const _x11 = fnPolymorphic(_x10);
      const x11 = declassify(_x11);
    });
    A.publish(x10, x, x2, x11);
    require(x10 == 10);
    require(x == 1);
    require(x2 == 2);
    require(x11 == 11);
    commit();
  }
);
