'reach 0.1';

export const main = Reach.App(() => {
  const M = Data({S : UInt, T : UInt});
  const A = Participant('A', { tok: Token });

  deploy();

  A.only(() => { const tok = declassify(interact.tok); });
  A.publish(tok);
  commit();

  A.publish().pay([10, [10, tok]]);
  commit();

  A.only(() => {const x = M.T(10);});
  A.publish(x).pay(
    x.match({
      T: (v) => { return [v, [10,tok]]; },
      S: (v) => { return [10, [10,tok]]; }
    })
  );
  commit();

  A.publish();
  transfer(10).to(this);
  transfer(10, tok).to(this);
  commit();

  exit();
});
