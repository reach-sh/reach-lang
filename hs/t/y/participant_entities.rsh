'reach 0.1';

/*
  Allow developers to specify more clearly
  what kind of entity a participant is.
*/

export const main =
  Reach.App(
    {},
    [
      Participant('A', { x: UInt }),
      ParticipantClass('B', { getY: Fun([], UInt) }),
    ],
    (A, B) => {
      exit(); }
  );
