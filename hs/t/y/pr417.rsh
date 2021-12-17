'reach 0.1';

export const main = Reach.App(() => {
  const M = Data({S : UInt, T : UInt});
  const A = Participant('A', { tok: Token });

  deploy();

  A.only(() => { const tok = declassify(interact.tok); });
  A.publish(tok);
  commit();

  A.only(() => {const x = M.T(10);});
  A.publish(x).pay(
    [ x.match({
        T: (v) => v,
        S: (v) => 10
      }),
      [ 10, tok ]
    ]
  );
  commit();

  A.publish();
  transfer(balance()).to(this);
  transfer(balance(tok), tok).to(this);
  commit();

  exit();
});
