'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    when: Fun([], Bool)
  });
  const B = API('B', { go: Fun([], Null) });

  init();

  A.publish();

  const [ a ] =
    parallelReduce([ true ])
    .invariant(balance() == 0)
    .while(a)
    .case(A,
      () => ({
         when: declassify(interact.when()),
      }),
      (_) => {
        return [ false ];
      })
    .api(B.go,
      (k) => {
        k(null);
        return [ 1 ];
      })
    .timeout(false);

  commit();

});
