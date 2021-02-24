'reach 0.1';

const Player =
      { getHand: Fun([], UInt),
        seeOutcome: Fun([UInt], Null) };
const Alice =
      { ...Player,
        wager: UInt };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt], Null) };

export const main =
  Reach.App(
    {},
    [Participant('Alice', Alice), Participant('Bob', Bob)],
    (A, B) => {
      A.only(() => {
        const wager = declassify(interact.wager);
        const handA = declassify(interact.getHand()); });
      A.publish(wager, handA)
        .pay(wager);
      commit();

      B.only(() => {
        interact.acceptWager(wager);
        const handB = (handA + 1) % 3; });
      B.publish(handB)
        .pay(wager);

      const outcome = (handA + (4 - handB)) % 3;
      require(handB == (handA + 1) % 3);
      assert(outcome == 0);
      const [forA, forB] =
            // was: outcome == 0 ? [0, 2] :
            outcome == 0 ? [0, 1] : // <-- Oops
            outcome == 1 ? [1, 1] :
            [2, 0];
      transfer(forA * wager).to(A);
      transfer(forB * wager).to(B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
