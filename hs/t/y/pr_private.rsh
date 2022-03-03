'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    when: Fun([], Bool),
    sec1: UInt,
    sec2: Bool,
    seeP: Fun([UInt,Bool], Null)
  });
  init();

  A.publish();

  const [ alive ] =
    parallelReduce([ true ])
      .invariant(balance() == 0)
      .while(alive)
      .case(A,
        () => {
          const _sec1 = interact.sec1;
          const _sec2 = interact.sec2;
          return {
            when: declassify(interact.when()),
            _local: [_sec1, _sec2]
          };
        },
        (_, [_sec1, _sec2]) => {
          A.only(() => {
            interact.seeP(_sec1, _sec2);
          });
          return [ true ];
      })
      .timeout(relativeSecs(25), () => {
        Anybody.publish();
        return [ false ];
      });

  commit();

});
