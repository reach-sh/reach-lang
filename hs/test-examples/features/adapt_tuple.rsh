'reach 0.1';

export const main =
  Reach.App(
    {},
    [ ['Alice', {} ],
      ['Bob', {} ],
      ['class', 'CoolPeople', {} ],
    ],
    (A, B, CP) => {
      A.publish();
      commit();
      B.publish();
      commit();
      CP.publish();
      commit();
      exit(); });
