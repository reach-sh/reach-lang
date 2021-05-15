'reach 0.1';

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      ...hasRandom,
      look: Fun([
        UInt, UInt, UInt, UInt,
        UInt, UInt, UInt, UInt,
        UInt, UInt, UInt, UInt,
        UInt, UInt, UInt, UInt,
        UInt,
      ], Null),
    }),
  ],
  (A) => {
    A.only(() => {
      const v0 = declassify(interact.random());
      const v1 = declassify(interact.random());
      const v2 = declassify(interact.random());
      const v3 = declassify(interact.random());
      const v4 = declassify(interact.random());
      const v5 = declassify(interact.random());
      const v6 = declassify(interact.random());
      const v7 = declassify(interact.random());
      const v8 = declassify(interact.random());
      const v9 = declassify(interact.random());
      const vA = declassify(interact.random());
      const vB = declassify(interact.random());
      const vC = declassify(interact.random());
      const vD = declassify(interact.random());
      const vE = declassify(interact.random());
      const vF = declassify(interact.random());
    });

    A.publish(
      v0,
      v1,
      v2,
      v3,
      v4,
      v5,
      v6,
      v7,
      v8,
      v9,
      vA,
      vB,
      vC,
      vD,
      vE,
      vF
    );
    const vs =
      (v0 % 2)
    + (v1 % 2)
    + (v2 % 2)
    + (v3 % 2)
    + (v4 % 2)
    + (v5 % 2)
    + (v6 % 2)
    + (v7 % 2)
    + (v8 % 2)
    + (v9 % 2)
    + (vA % 2)
    + (vB % 2)
    + (vC % 2)
    + (vD % 2)
    + (vE % 2)
    + (vF % 2);

    if ( vs % 2 == 0 ) {
      commit();
      A.publish();
    }
    commit();

    A.only(() => interact.look(
      v0,
      v1,
      v2,
      v3,
      v4,
      v5,
      v6,
      v7,
      v8,
      v9,
      vA,
      vB,
      vC,
      vD,
      vE,
      vF,
      vs
    ));
    exit();
  }
);
