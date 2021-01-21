'reach 0.1';

/*
  Allow developers to specify more clearly
  what kind of entity a participant is.

  This could be implemented in stdlib, however
  Participant would overshadow builtin keyword.
*/

const Participant = (participantName, participantInterface) => [
  participantName, participantInterface
];

const Class = (participantName, participantInterface) => [
  'class', participantName, participantInterface
];

const participants = [
  Participant('A', { x: UInt }),
  Class('B', { getY: Fun([], UInt) }),
];

export const main =
  Reach.App(
    {},
    participants,
    (A, B) => {
      exit(); }
  );
