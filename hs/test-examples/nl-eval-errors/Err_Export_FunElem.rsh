'reach 0.1';

const f = () => 1;

export const x = [is(f, Fun([], UInt))];

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      exit(); });
