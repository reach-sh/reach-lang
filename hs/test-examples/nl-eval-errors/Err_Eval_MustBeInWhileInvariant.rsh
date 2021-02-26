'reach 0.1';

export const main =
  Reach.App({},
    [ Participant('A', {}) ],
    (A) => {
      A.publish();
      const m = new Map(UInt);
      m.reduce(0, (acc, x) => acc + x);
    });
