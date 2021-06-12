'reach 0.1';

const fundraiserApi = {
  projectName: Bytes(64),
  fundraisingGoal: UInt,
  contractDuration: UInt,
};

const contributorApi = {
  getContribution: Fun([], UInt),
};

const myFromMaybe = (amt) => fromMaybe(amt, (() => 0), ((x) => x));

export const main = Reach.App(() => {
  const F = Participant('Fundraiser', fundraiserApi);
  const C = ParticipantClass('Contributor', contributorApi);
  deploy();

  F.only(() => {
    const p = {
      name: declassify(interact.projectName),
      goal: declassify(interact.fundraisingGoal),
      duration: declassify(interact.contractDuration)
    }
  });

  F.publish(p);

  const ctMap = new Map(UInt);
  const [sum, stop] = parallelReduce([0, false])
    .invariant(balance() == sum)
    .while(! stop && balance() < p.goal)
    .case(C, (() => {
        const amt = declassify(interact.getContribution());
        return { when: amt > 0, msg: amt };
      }),
      ((amt) => amt),
      ((amt) => {
        const winner = this;
        ctMap[winner] = myFromMaybe(ctMap[winner]) + amt;
        return [sum + amt, false];
      })
    )
    .timeout(p.duration, () => {
      Anybody.publish();
      return [sum, true];
    });

  if ( sum < p.goal ) {
    var [bal] = [sum];
    invariant(balance() == bal && bal == ctMap.sum());
    while (bal > 0) {
      commit();
      C.only(() => {
        const didContribute = myFromMaybe(ctMap[this]) > 0;
      });
      C.publish().when(didContribute).timeout(false);
      const refund = myFromMaybe(ctMap[this]);
      transfer(refund).to(this);
      delete ctMap[this];
      bal = bal - refund;
      continue;
    }
  } else {
    transfer(sum).to(F);
  }

  commit();
  exit();
});
