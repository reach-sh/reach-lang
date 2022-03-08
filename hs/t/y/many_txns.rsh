'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: UInt,
    tok0: Token,
    tok1: Token,
    tok2: Token,
    tok3: Token,
    tok4: Token,
    tok5: Token,
    tok6: Token,
    tok7: Token,
    tok8: Token,
    tok9: Token,
    tokA: Token,
    tokB: Token,
    tokC: Token,
    tokD: Token,
    tokE: Token,
    tokF: Token,
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
    const tok0 = declassify(interact.tok0);
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    const tok3 = declassify(interact.tok3);
    const tok4 = declassify(interact.tok4);
    const tok5 = declassify(interact.tok5);
    const tok6 = declassify(interact.tok6);
    const tok7 = declassify(interact.tok7);
    const tok8 = declassify(interact.tok8);
    const tok9 = declassify(interact.tok9);
    const tokA = declassify(interact.tokA);
    const tokB = declassify(interact.tokB);
    const tokC = declassify(interact.tokC);
    const tokD = declassify(interact.tokD);
    const tokE = declassify(interact.tokE);
    const tokF = declassify(interact.tokF);
    assume(distinct(
      tok0,
      tok1,
      tok2,
      tok3,
      tok4,
      tok5,
      tok6,
      tok7,
      tok8,
      tok9,
      tokA,
      tokB,
      tokC,
      tokD,
      tokE,
      tokF
    ));
  });
  A.publish(x,
    tok0,
    tok1,
    tok2,
    tok3,
    tok4,
    tok5,
    tok6,
    tok7,
    tok8,
    tok9,
    tokA,
    tokB,
    tokC,
    tokD,
    tokE,
    tokF
  );
  commit();
  const p = [x,
    [x, tok0],
    [x, tok1],
    [x, tok2],
    [x, tok3],
    [x, tok4],
    [x, tok5],
    [x, tok6],
    [x, tok7],
    [x, tok8],
    [x, tok9],
    [x, tokA],
    [x, tokB],
    [x, tokC],
    [x, tokD],
    [x, tokE],
    [x, tokF],
  ];
  A.pay(p);
  transfer(p).to(A);
  commit();
  exit();
});
