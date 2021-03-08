'reach 0.1';

const Player = (Move, Outcome) =>
      ({ ...hasRandom,
         getMove: Fun([], Move),
         seeOutcome: Fun([Outcome], Null),
         informTimeout: Fun([], Null) });
const Alice = (Move, Outcome) =>
      ({ ...Player(Move, Outcome),
         terms: UInt });
const Bob = (Move, Outcome) =>
      ({ ...Player(Move, Outcome),
         acceptTerms: Fun([UInt], Null) });

const simultaneousLoop =
      (A, B, DEADLINE, isOutcome, outcome0, stop, combine, divide) => {
        const informTimeout = () => {
          each([A, B], () => {
            interact.informTimeout(); }); };

        A.only(() => {
          const terms = declassify(interact.terms); });
        A.publish(terms)
          .pay(terms);
        commit();

        B.only(() => {
          interact.acceptTerms(terms); });
        B.pay(terms)
          .timeout(DEADLINE, () => closeTo(A, informTimeout));

        var outcome = outcome0;
        invariant(balance() == 2 * terms && isOutcome(outcome));
        while ( ! stop(outcome) ) {
          commit();

          A.only(() => {
            const _moveA = interact.getMove();
            const [_commitA, _saltA] = makeCommitment(interact, _moveA);
            const commitA = declassify(_commitA); });
          A.publish(commitA)
            .timeout(DEADLINE, () => closeTo(B, informTimeout));
          commit();

          unknowable(B, A(_moveA, _saltA));
          B.only(() => {
            const moveB = declassify(interact.getMove()); });
          B.publish(moveB)
            .timeout(DEADLINE, () => closeTo(A, informTimeout));
          commit();

          A.only(() => {
            const [saltA, moveA] = declassify([_saltA, _moveA]); });
          A.publish(saltA, moveA);
          checkCommitment(commitA, saltA, moveA);

          outcome = combine(moveA, moveB);
          continue; }

        const [ toA, toB ] = divide(outcome);
        transfer(toA * terms).to(A);
        transfer(toB * terms).to(B);
        commit();
        exit();
      };

// RPS

const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);
const [ isRPSOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);
const winner = (handA, handB) =>
      ((handA + (4 - handB)) % 3);

export const rps =
  Reach.App(
    {},
    [Participant('Alice', Alice(UInt, UInt)), Participant('Bob', Bob(UInt, UInt))],
    (A, B) =>
    simultaneousLoop(
      A, B,
      10, isRPSOutcome, DRAW, ((o) => (o != DRAW)), winner,
      ((o) => (o == A_WINS ? [ 2, 0 ] : [ 0, 2 ]))));

// Rental Agreement

const [ isRentalOutcome, NONE, ONLY_L, ONLY_T, BOTH ] = makeEnum(4);

export const rental =
  Reach.App(
    {},
    [Participant('Landlord', Alice(Bool, UInt)), Participant('Tenant', Bob(Bool, UInt))],
    (A, B) =>
    simultaneousLoop(
      A, B,
      10, isRentalOutcome, NONE, ((o) => (o != NONE)),
      ((l, t) => (l && t ? BOTH :
                  l ? ONLY_L :
                  t ? ONLY_T :
                  NONE)),
      ((o) => (o == BOTH ? [ 1, 1 ] :
               o == ONLY_L ? [ 0, 2 ] :
               [ 2, 0 ]))));
