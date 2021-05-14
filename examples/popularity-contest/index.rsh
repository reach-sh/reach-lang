'reach 0.1';
'use strict';

const [ _, ALICE_WINS, BOB_WINS, TIMEOUT ] = makeEnum(3);

const Common = {
  showOutcome: Fun([UInt, UInt, UInt], Null),
};

export const main =
  Reach.App(() => {

    setOptions({ connectors: [ETH, ALGO ]});

    const Pollster =
      Participant('Pollster', { ...Common,
        getParams: Fun([], Object({
          ticketPrice: UInt,
          deadline: UInt,
          aliceAddr: Address,
          bobAddr: Address }))
      });

    const Voter =
      ParticipantClass('Voter',
      { ...Common,
        getVote: Fun([], Bool),
        voterWas: Fun([Address], Null),
        shouldVote: Fun([], Bool),
      });

    deploy();

    const showOutcome = (which, forA, forB) => () => {
      each([Pollster, Voter], () =>
        interact.showOutcome(which, forA, forB)); };

    Pollster.only(() => {
      const { ticketPrice, deadline, aliceAddr, bobAddr } =
        declassify(interact.getParams());
    });
    Pollster.publish(ticketPrice, deadline, aliceAddr, bobAddr);

    const [ timeRemaining, keepGoing ] = makeDeadline(deadline);

    const [ forA, forB ] =
      parallelReduce([ 0, 0])
      .invariant(balance() == (forA + forB) * ticketPrice)
      .while( keepGoing() )
      .case(Voter, (() => ({
          msg: declassify(interact.getVote()),
          when: declassify(interact.shouldVote()),
        })),
        ((_) => ticketPrice),
        ((forAlice) => {
          const voter = this;
          Voter.only(() => interact.voterWas(voter));
          const [ nA, nB ] = forAlice ? [ 1, 0 ] : [ 0, 1 ];
          return [ forA + nA, forB + nB ]; }))
      .timeout(timeRemaining(), () => {
        Anybody.publish();
        showOutcome(TIMEOUT, forA, forB)();
        return [ forA, forB ]; });

    const outcome = forA >= forB ? ALICE_WINS : BOB_WINS;
    const winner = outcome == ALICE_WINS ? aliceAddr : bobAddr;
    transfer(balance()).to(winner);
    commit();
    showOutcome(outcome, forA, forB)();

  });

