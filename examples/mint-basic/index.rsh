'reach 0.1';

export const main = Reach.App(() => {
  const shared = {
    showToken: Fun([Token], Null),
    didTransfer: Fun([Bool, UInt], Null),
  };
  const A = Participant('Alice', {
    getParams: Fun([], Object({
      name: Bytes(32), symbol: Bytes(8),
      url: Bytes(32), metadata: Bytes(32),
      supply: UInt,
      amt: UInt,
      doEarlyTransfer: Bool,
    })),
    ...shared,
  });
  const B = Participant('Bob', {
    ...shared,
  });
  deploy();

  A.only(() => {
    const { name, symbol, url, metadata, supply, amt, doEarlyTransfer } = declassify(interact.getParams());
    assume(2 * amt <= supply);
    assume(2 * amt <= UInt.max);
  });
  A.publish(name, symbol, url, metadata, supply, amt, doEarlyTransfer);
  require(2 * amt <= supply);
  require(2 * amt <= UInt.max);

  const tok1 = new Token({name, symbol, url, metadata, supply});
  /*
  if ( doEarlyTransfer ) {
    // We will never do this, but it is here to force a warning to be generated
    transfer([[amt, tok1]]).to(A);
    commit();
    A.pay([amt, tok1]);
  }
  */
  A.interact.showToken(tok1);
  commit();

  B.publish();
  B.interact.showToken(tok1);
  commit();

  const doTransfer = (tokX) => {
    const doTransfer1 = (who) => {
      transfer(amt, tokX).to(who);
      who.interact.didTransfer(true, amt);
    };
    doTransfer1(A);
    doTransfer1(B);
  };

  A.publish();
  doTransfer(tok1);
  commit();

  A.pay([[amt, tok1]]);
  commit();
  B.pay([[amt, tok1]]);
  // XXX tok1.burn(supply);
  // XXX tok1.destroy();

  const tok2 = new Token({name, symbol});
  A.interact.showToken(tok2);
  B.interact.showToken(tok2);
  commit();

  A.publish();
  doTransfer(tok2);
  // XXX tok2.burn(/* defaults to all */);
  commit();

  exit();
});
