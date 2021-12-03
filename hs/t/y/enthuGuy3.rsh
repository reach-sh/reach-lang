'reach 0.1';
export const main = Reach.App(() => {
  const S = Struct([ ['y', UInt] ]);
  const A = Participant('A', {});
  const P = API('P', { f: Fun([S],Bool) });
  deploy();
  A.publish();
  const keepGoing =
    parallelReduce(true)
    .invariant(balance() == 0)
    .while(keepGoing)
    .case(A, (() => ({})), ((msg) => { return true; }))
    .api(P.f, (a, k) => {
        k(false);
        return false;
    });
  commit();
  exit();
});
