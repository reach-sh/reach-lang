'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const s = Struct([["$x ", UInt]]);
    });
