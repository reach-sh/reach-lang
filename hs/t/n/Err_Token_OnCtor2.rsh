'reach 0.1';
'use strict';

export const main =
  Reach.App(
    {},
    [ Participant('A', {
        get: Fun([], Tuple(Token, UInt)),
      }),
    ],
    (A) => {
      A.only(() => {
        const [tok, amt] = declassify(interact.get()); });
      A.publish(tok, amt)
        .pay([amt, [amt, tok], [amt, tok]]);
      commit();
      exit();
    }
  );

