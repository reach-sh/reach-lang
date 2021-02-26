'reach 0.1';

export const main =
  Reach.App({},
    [ Participant('A', {}) ],
    (A) => {
      Map.reduce(0, 1, 2);
    });
