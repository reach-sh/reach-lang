'reach 0.1';

export const main = Reach.App(() => {
  const shared = {
    showToken: Fun(true, Null),
    didTransfer: Fun([Bool, UInt], Null),
  };
  const A = Participant('Alice', {
    getParams: Fun([], Object({
      name: Bytes(32), symbol: Bytes(8),
      url: Bytes(96), metadata: Bytes(32),
      supply: UInt,
      amt: UInt,
    })),
    ...shared,
  });
  const B = Participant('Bob', {
    ...shared,
  });
  const TLE = Events({ tokenLaunch: [] });
  init();

  A.only(() => {
    const { name, symbol, url, metadata, supply, amt } = declassify(interact.getParams());
    assume(4 * amt <= supply);
    assume(4 * amt <= UInt.max);
  });
  A.publish(name, symbol, url, metadata, supply, amt);
  require(4 * amt <= supply);
  require(4 * amt <= UInt.max);

  const md1 = {name, symbol, url, metadata, supply};
  const tok1 = new Token(md1);
  TLE.tokenLaunch();
  A.interact.showToken(tok1, md1);
  commit();

  B.publish();
  B.interact.showToken(tok1, md1);
  commit();

  const doTransfer1 = (who, tokX) => {
    transfer(2 * amt, tokX).to(who);
    who.interact.didTransfer(true, amt);
  };

  B.publish();
  doTransfer1(B, tok1);
  commit();
  A.publish();
  doTransfer1(A, tok1);
  commit();
  A.pay([[2*amt, tok1]]);
  commit();
  B.pay([[2*amt, tok1]]);
  tok1.burn(supply);
  tok1.destroy();

  const md2 = {name, symbol};
  const tok2 = new Token(md2);
  TLE.tokenLaunch();
  A.interact.showToken(tok2, md2);
  B.interact.showToken(tok2, md2);
  commit();

  B.publish();
  doTransfer1(B, tok2);
  commit();
  A.publish();
  doTransfer1(A, tok2);
  tok2.burn(/* defaults to all */);
  commit();
  A.pay([[2*amt, tok2]]);
  commit();
  B.pay([[2*amt, tok2]]);
  tok2.burn();
  tok2.destroy();
  commit();

  exit();
});
