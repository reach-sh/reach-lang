'reach 0.1';

const Args = Struct([
  ["argL", UInt],
  ["argR", UInt],
]);

const mkApp = (op) => Reach.App(() => {
  const V = View({ args: Args });
  const D = Participant('D', {
    argL: UInt,
    argR: UInt,
    ready: Fun([], Null),
  });
  const A = API({ go: Fun([], UInt) });
  init();
  D.only(() => {
    const argL = declassify(interact.argL);
    const argR = declassify(interact.argR);
  });
  D.publish(argL, argR);
  V.args.set(Args.fromObject({argL, argR}));
  commit();
  D.interact.ready();
  const [ [], k ] = call(A.go);
  k(op(argL, argR));
  commit();
});

export const dapp_plus = mkApp((x, y) => x + y);
export const dapp_sub = mkApp((x, y) => {
  if (x > y) { return x - y; } else { return 0; }
});
export const main = dapp_plus;
