'reach 0.1';
export const main = Reach.App(() => {
  const D = Participant('D', {});
  const P = API('P', { f: Fun([Address], Bool) });
  deploy();
  D.publish();
  const m = new Map(Struct([["p", Bool]]));
  const kg =
    parallelReduce(true)
    .invariant(balance() == 0)
    .while(kg)
    .api(P.f, (a, k) => {
      k(m[a].match({
        None: () => false,
        Some: (o) => (o.p === true),
      }));
      return false;
    });
  commit();
  exit();
});
