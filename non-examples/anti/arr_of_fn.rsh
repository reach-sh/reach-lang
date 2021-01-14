'reach 0.1';

const fns = [(x) => x, (x) => x + 1];

export const main = Reach.App(
  {},
  [["A", {
    arg: UInt,
    showRes: Fun([UInt], Null),
  }]],
  (A) => {
    A.only(() => {
      const arg = interact.arg;
      const xs = Array.map(fns, (f) => f(arg));
      // ^ reachc: error:
      // ./arr_of_fn.rsh:14:27:application: TypeError:
      // Value cannot exist at runtime: [<closure> ]
      const res = Array.sum(xs);
      interact.showRes(res);
    });
  };
);
