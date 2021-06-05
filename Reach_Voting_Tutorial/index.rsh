'reach 0.1';

const [ isVote, NO, YES] = makeEnum(2);
const [ finalResult, FAIL, DRAW, SUCCESS ] = makeEnum(3);

const consensus = (voteA, voteB) =>
      voteA + voteB;

assert(consensus(NO, YES) ==  DRAW);
assert(consensus(YES, YES) == SUCCESS);
assert(consensus(NO, NO) == FAIL);

const Voter =
      { ...hasRandom,
        getVote: Fun([], UInt),
        seeResult: Fun([UInt], Null),
        informTimeout: Fun([], Null) };
const Alice =
      { ...Voter,
        stake: UInt };
const Bob =
      { ...Voter,
        acceptStake: Fun([UInt], Null) };

const DEADLINE = 30;

export const main =
  Reach.App(
    {},
    [Participant('Alice', Alice), Participant('Bob', Bob)],
    (A, B) => {
      const informTimeout = () => {
        each([A, B], () => {
          interact.informTimeout(); }); };
      A.only(() => {
        const stake = declassify(interact.stake); });
      A.publish(stake)
        .pay(stake);
      commit();
      B.only(() => {
        interact.acceptStake(stake); });
      B.pay(stake)
        .timeout(DEADLINE, () => closeTo(A, informTimeout));
      var result = DRAW;
      invariant(balance() == 2 * stake);
      while ( result == DRAW ) {
        commit();
        A.only(() => {
          const _voteA = interact.getVote();
          const [_decisionA, _saltA] = makeCommitment(interact, _voteA);
          const decisionA = declassify(_decisionA); });
        A.publish(decisionA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        commit();
        unknowable(B, A(_voteA, _saltA));
        B.only(() => {
          const voteB = declassify(interact.getVote()); });
        B.publish(voteB)
          .timeout(DEADLINE, () => closeTo(A, informTimeout));
        commit();
        A.only(() => {
          const [saltA, voteA] = declassify([_saltA, _voteA]); });
        A.publish(saltA, voteA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        checkCommitment(decisionA, saltA, voteA);
        result = consensus(voteA, voteB);
        continue; }
      transfer(2 * stake).to(result == SUCCESS ? A : B);
      commit();
      each([A, B], () => {
        interact.seeResult(result); });
      exit(); });