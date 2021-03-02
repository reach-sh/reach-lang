'reach 0.1';

export const main =
  Reach.App(
    {},
    [ Participant('A', { getZ: Fun([], Array(UInt, 4)) }) ],
    (A) => {
      A.only(() => {
        const x = 0; });
      A.publish(x);
      commit();

      A.only(() => {
        const z =
          declassify(interact.getZ()).map((a) => lastConsensusTime() + a);
        const y = lastConsensusTime();
      });
      A.publish(y);
      commit();

      exit();
    });
