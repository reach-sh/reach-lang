'reach 0.1';

const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);
const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);

const winner = (handAlice, handBob) =>
  ((handAlice + (4 - handBob)) % 3);

assert(winner(ROCK, PAPER) == B_WINS);
assert(winner(PAPER, ROCK) == A_WINS);
assert(winner(ROCK, ROCK) == DRAW);

forall(UInt, handAlice =>
  forall(UInt, handBob =>
    assert(isOutcome(winner(handAlice, handBob)))));

forall(UInt, (hand) =>
  assert(winner(hand, hand) == DRAW));

const Player = {
  ...hasRandom, // <--- new!
  getHand: Fun([], UInt),
  seeOutcome: Fun([UInt], Null),
};

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...Player,
    wager: UInt,
  });
  const Bob   = Participant('Bob', {
    ...Player,
    acceptWager: Fun([UInt], Null),
  });
  init();

  Alice.only(() => {
    const wager = declassify(interact.wager);
    const _handAlice = interact.getHand();
    const [_commitAlice, _saltAlice] = makeCommitment(interact, _handAlice);
    const commitAlice = declassify(_commitAlice);
  });
  Alice.publish(wager, commitAlice)
    .pay(wager);
  commit();

  unknowable(Bob, Alice(_handAlice, _saltAlice));
  Bob.only(() => {
    interact.acceptWager(wager);
    const handBob = declassify(interact.getHand());
  });
  Bob.publish(handBob)
    .pay(wager);
  commit();

  Alice.only(() => {
    const saltAlice = declassify(_saltAlice);
    const handAlice = declassify(_handAlice);
  });
  Alice.publish(saltAlice, handAlice);
  checkCommitment(commitAlice, saltAlice, handAlice);

  const outcome = winner(handAlice, handBob);
  const                 [forAlice, forBob] =
    outcome == A_WINS ? [       2,      0] :
    outcome == B_WINS ? [       0,      2] :
    /* tie           */ [       1,      1];
  transfer(forAlice * wager).to(Alice);
  transfer(forBob   * wager).to(Bob);
  commit();

  each([Alice, Bob], () => {
    interact.seeOutcome(outcome);
  });
});
