'reach 0.1';

const Player = {
  ...hasRandom,
  getHand: Fun([], UInt),
};

export const main =
  Reach.App(
    {},
    [ ['Alice',
       { ...Player,
         wager: UInt } ],
      ['Bob',
       { ...Player,
         acceptWager: Fun([UInt], Null) } ],
    ],
    (Alice, Bob) => {

      Alice.only(() => {
        const wager = declassify(interact.wager);
      });
      Alice.publish(wager)
        .pay(wager);
      commit();

      Bob.only(() => {
        interact.acceptWager(wager); });
      Bob.pay(wager);
      commit();

      Alice.only(() => {
        const _handA = interact.getHand();
        const [ _commitA, _saltA ] =
          makeCommitment(interact, _handA);
        const commitA = declassify(_commitA); });
      Alice.publish(commitA);
      commit();

      unknowable(Bob, Alice(_handA, _saltA));
      Bob.only(() => {
        const handB = declassify(interact.getHand()); });
      Bob.publish(handB);
      commit();

      Alice.only(() => {
        const [ handA, saltA ] =
          declassify([ _handA, _saltA]); });
      Alice.publish(handA, saltA);
      checkCommitment(commitA, saltA, handA);

      const outcome = ((handA + (4 - handB)) % 3);
      const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);

      const [ toA, toB ] =
        outcome == B_WINS ? [ 0, 2 ] :
        outcome == A_WINS ? [ 2, 0 ] :
        [ 1, 1 ];
      transfer(toA * wager).to(Alice);
      transfer(toB * wager).to(Bob);
      commit();

      exit();
    }
  );
