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
    [['Alice', Alice], ['Bob', Bob]],
    (A, B) => {
      A.only(() => {
        const wager = declassify(interact.wager);
        const handA = declassify(interact.getHand()); });
      A.publish(wager, handA)
        .pay(wager);
      commit();

      B.only(() => {
        interact.acceptWager(wager);
        const handB = declassify(interact.getHand()); });
      B.publish(handB)
        .pay(wager);

      const outcome = (handA + (4 - handB)) % 3;
      const [forA, forB] =
            outcome == 2 ? [2, 0] :
            outcome == 0 ? [0, 2] :
            [1, 1];
      transfer(forA * wager).to(A);
      transfer(forB * wager).to(B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
