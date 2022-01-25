'reach 0.1';
'use strict';
export const main = Reach.App(() => {
  const V = View('V',{ t: Token });
  const T = API('T', { F: Fun([UInt], Null) });
  const G = Participant('G', { ready: Fun([], Null) });
  const O = Participant('O', {});
  init();
  G.publish();
  const tok1 = new Token({});
  const tok2 = new Token({});
  V.t.set(tok2);
  G.interact.ready();
  commit();
  O.publish();
  const x = parallelReduce(balance(tok2))
    .invariant(balance(tok2) == x)
    .while(true)
    .paySpec([tok1, tok2])
    .api(T.F,
      ((amt) => { assume(amt <= balance(tok2)); }),
      ((amt) => [ 0, [amt, tok1], [0, tok2] ]),
      (amt, k) => {
        require(amt <= balance(tok2));
        const X = this;
        k(null);
        commit();
        G.publish();
        transfer(amt, tok1).to(O);
        transfer(amt, tok2).to(X);
        return x - amt;
    })
    .timeout(false);
  commit();
  exit();
});
