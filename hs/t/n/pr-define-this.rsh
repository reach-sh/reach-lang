'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const P = API({ f: Fun([], Bool) });
  init();
  A.publish();
  const [] = parallelReduce([])
    .define(() => { const cool = this; });
    .invariant( balance() == 0 )
    .while(true)
    .api_(P.f, () => {
      return [ 0, (k) => {
        k(true);
        return [];
      }];
    })
  commit();
  exit();
});
