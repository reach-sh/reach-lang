'reach 0.1';

import { bountyFunction } from 'bountyFunction.rsh';

// interact interface for Funder
const FunderInterface =
{
    getBounty: Fun([], Object({
        deadline: UInt,
        amt: UInt,
    })),
    postWager: Fun([], Null)
}

// interact interface for Contestant participating
// in the contest
const ContestantInterface =
{
    informBounty: Fun([UInt, UInt], Null),
    submitValue: Fun([], Maybe(UInt)),
    informWinner: Fun([Address], Null),
    informSuccess: Fun([Bool, Address], Null)
}

// interact interface for an observer Participant
// whose work is to observe successful submissions
// and report them to all Web frontends so this 
// data can be used to form a leaderboard
const MonitorInterface =
{
    seeSubmission: Fun([Address, UInt, UInt], Null)
}

export const main =
    Reach.App(
        {},
        [Participant('Funder', FunderInterface), ParticipantClass('Contestant', ContestantInterface), ParticipantClass('Monitor', MonitorInterface)],
        (Funder, Contestant, Monitor) => {

            // Get the bounty details
            Funder.only(() => {
                const { amt, deadline } = declassify(interact.getBounty());
            });

            Funder.publish(amt, deadline)
                .pay(amt);

            // Used to inform the Web frontend that wager
            // deployment is finished
            Funder.only(() => interact.postWager());

            // Inform all contestant frontends about the
            // wager details
            each([Contestant], () => {
                interact.informBounty(amt, deadline);
            });

            // maintain the current leader in currentWinner
            const [keepGoing, currentWinner] =
                // initialise with Funder's account so that the amount
                // is reverted to Funder in case of no submissions
                parallelReduce([true, { accountAddress: Funder, returnValue: 0, inputValue: 0}])
                    .invariant(balance() == amt)
                    .while(keepGoing)
                    .case(
                        Contestant,
                        (() => {
                            const value = declassify(interact.submitValue());
                            return {
                                // proceed in case some value is submitted by
                                // a participant
                                when: isSome(value),
                                msg: value
                            }
                        }),
                        ((msg) => {
                            const currentContestant = this;
                            const inputValue = fromSome(msg, 0);
                            // Evaluate currentContestant's submitted value in
                            // the bounty function provided by the Funder
                            const evaluatedValue = bountyFunction(inputValue);
                            // inform the Monitor oberver frontend Participant 
                            // about the current submission 
                            Monitor.only(() => interact.seeSubmission(currentContestant, inputValue, evaluatedValue))

                            const newEntry = {
                                accountAddress: currentContestant,
                                inputValue: inputValue,
                                returnValue: evaluatedValue,
                            };

                            // Check if the current submission gives a better
                            // output than the leader
                            const newWinner = evaluatedValue > currentWinner.returnValue ? newEntry : currentWinner;

                            // inform which Conntestant won the race so other
                            // Contestants can continue polling till their 
                            // value is accepted
                            Contestant.only(() => interact.informSuccess(true, currentContestant));
                            return [true, newWinner];
                        })
                    )
                    .timeout(deadline, () => {
                        Anybody.publish();
                        return [false, currentWinner];
                    });

            // pay the current winner
            transfer(balance()).to(currentWinner.accountAddress);
            commit();

            exit();
        }
    );
