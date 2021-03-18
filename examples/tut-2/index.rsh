'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', {}), Participant('Bob', {})],
    (A, B) => {
      exit(); });
