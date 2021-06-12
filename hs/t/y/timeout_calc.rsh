'reach 0.1';

export const main =
  Reach.App(
    {},
    [ Participant('A', { getX: Fun([], UInt) }) ],
    (A) => {
      A.only(() => {
        const x = declassify(interact.getX());
        const y = declassify(interact.getX()); });
      A.publish(x, y);
      commit();

      A.publish()
       .timeout(x + y, () => {
         A.publish();
         commit();
         exit(); });
      commit();
      exit();
    });
