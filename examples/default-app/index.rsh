'reach 0.1';

export const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);
export const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);

export const winner = (handA, handB) =>
      ((handA + (4 - handB)) % 3);

forall(UInt, handA =>
  forall(UInt, handB =>
    assert(isOutcome(winner(handA, handB)))));


const winnerType = Fun([UInt, UInt], UInt);
const preCond = ([l, r]) => isHand(l) && isHand(r);
const postCond = ([l, r], res) => isHand(res) && isOutcome(res);

export const winnerIs = is(winner, Refine(winnerType, preCond, postCond));
