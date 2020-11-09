'reach 0.1';

const [ isOutcome, ALICE_WINS, BOB_WINS ] = makeEnum(2);

const Common = {
  showOutcome: Fun([UInt], Null)
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [['Pollster',
      { ...Common,
        getParams: Fun([], Object({ ticketPrice: UInt,
                                    deadline: UInt,
                                    aliceAddr: Address,
                                    bobAddr: Address })) }],
     ['class', 'Voter',
      { ...Common,
        confirmVote: Fun([UInt], Bool) } ],
    ],
    (Pollster, Voter) => {
      Pollster.only(() => {
        const { ticketPrice, deadline, aliceAddr, bobAddr } =
          declassify(interact.getParams());
      });
      Pollster.publish(ticketPrice, deadline, aliceAddr, bobAddr);
      commit();

      var [ forA, forB ] = [ 0, 0 ];
      deadline(deadline);
      invariant(balance() == (forA + forB) * ticketPrice),
      for ( const aVoter in Voter ) {
        Voter.only(() => {
          const vote =
            declassify(interact.confirmVote(ticketPrice)); });
        Voter.publish(vote)
         .pay(ticketPrice);
        const [ nA, nB ] = vote ? [ 1, 0 ] : [ 0, 1 ];
        [ forA, forB ] = [ forA + nA, forB + nB ];
        continue;
      }

      const outcome = forA >= forB ? ALICE_WINS : BOB_WINS;
      const winner = outcome == ALICE_WINS ? aliceAddr : bobAddr;
      transfer(balance()).to(winner);
      commit();

      each([Pollster, Voter], () => {
        interact.showOutcome(outcome); } );
    });
