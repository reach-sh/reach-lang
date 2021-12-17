'reach 0.1';
'use strict';
export const main = Reach.App(() => {
  const A = Participant('A', { });
  const B = ParticipantClass('B', { });
  deploy();
  A.publish();
  const xM = new Map(UInt);
  const x = parallelReduce(0)
    .invariant(
      balance() == 0
      && xM.all((_) => true)
    )
    .while(x < 2)
    .case(B, (() => ({ msg: null })), ((_) => { return x; }))
    .timeout(false);
  const y = parallelReduce(0)
    .invariant(
      balance() == 0
      && xM.sum() == x
    )
    .while(y < x)
    .case(B, (() => ({ msg: null })), ((_) => { return y; }))
    .timeout(false);
  commit();
});
