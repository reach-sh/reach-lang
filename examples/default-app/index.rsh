'reach 0.1';

export const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);
export const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);

export const winner = (handAlice, handBob) =>
      ((handAlice + (4 - handBob)) % 3);

forall(UInt, handAlice =>
  forall(UInt, handBob =>
    assert(isOutcome(winner(handAlice, handBob)))));

const winnerType = Fun([UInt, UInt], UInt);
const preCond = ([l, r]) => isHand(l) && isHand(r);
const postCond = ([l, r], res) => isHand(res) && isOutcome(res);

export const winnerIs = is(winner, Refine(winnerType, preCond, postCond));
