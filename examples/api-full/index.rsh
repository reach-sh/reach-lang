'reach 0.1';
'use strict';

const State = Tuple(Bool, UInt, UInt, UInt);

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    ...hasConsoleLogger,
    tok: Token,
    amt: UInt,
  });
  const V = View('Reader', {
    read: State,
  });
  const U = API('Writer', {
    touch: Fun([UInt], State),
    writeN: Fun([UInt], State),
    writeT: Fun([UInt], State),
    writeB: Fun([UInt], State),
    writeX: Fun([UInt], State),
    end: Fun([], State),
  });
  init();
  A.only(() => {
    const [ tok, amt ] = declassify([ interact.tok, interact.amt ]);
  });
  A.publish(tok, amt)
  commit();
  A.pay([[amt, tok]]);
  A.interact.log("Ready!");

  const [ done, x, an, at ] =
    parallelReduce([ false, 0, 0, amt ])
    .invariant(balance() == an)
    .invariant(balance(tok) == at)
    .define(() => {
      V.read.set([done, x, an, at ]);
    })
    .while( ! done )
    .paySpec([tok])
    .api(U.touch, ((_) => [ 0, [ 0, tok ] ]), ((i, k) => {
        const stp = [ done, x + i, an, at ];
        k(stp);
        return stp;
    }))
    .api(U.writeX, ((i) => [ i, [ 0, tok ] ]), ((i, k) => {
        const stp = [ done, x + i, an + i, at ];
        k(stp);
        return stp;
    }))
    .api(U.writeN, ((_) => [ amt, [ 0, tok ] ]), ((i, k) => {
        const stp = [ done, x + i, an + amt, at ];
        k(stp);
        return stp;
    }))
    .api(U.writeT, ((i) => { assume(i > x); }), ((_) => [ 0, [ amt, tok ] ]), ((i, k) => {
        require(i > x);
        const stp = [ done, x + i, an, at + amt ];
        k(stp);
        return stp;
    }))
    .api(U.writeB, ((_) => [ amt, [ amt, tok ] ]), ((i, k) => {
        const stp = [ done, x + i, an + amt, at + amt ];
        k(stp);
        return stp;
    }))
    .api(U.end, (() => [ 0, [ 0, tok ] ]), ((k) => {
        const stp = [ true, x, an, at ];
        k(stp);
        return stp;
    }));

  transfer(an).to(A);
  transfer(at, tok).to(A);
  commit();

  exit();
});
