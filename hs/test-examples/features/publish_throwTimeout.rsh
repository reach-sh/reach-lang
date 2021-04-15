'reach 0.1';

const common = {
  onTimeout: Fun([UInt], Null) };

export const main =
  Reach.App(
    {},
    [
      Participant('A', { ...common }),
      Participant('B', { ...common })
    ],
    (A, B) => {

      const informTimeout = (n) =>
        each([A, B], () => {
          assert(n == 5);
          interact.onTimeout(n) });

      B.publish();
      commit();

      A.publish();
      commit();

      try {
        A.pay(10).throwTimeout(10, 5);
        transfer(10).to(B);
        commit();
      } catch (e) {
        closeTo(A, () => informTimeout(e)); }
    });
