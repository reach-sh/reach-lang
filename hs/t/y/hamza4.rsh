'reach 0.1';
const TokenParams = Object({
  name: Bytes(32),
  symbol: Bytes(8),
  url: Bytes(96),
  metadata: Bytes(32),
  supply: UInt,
  decimals: UInt,
});

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    doNothing: Fun([], Null),
    getParams: Fun([], TokenParams)
  });
  const B = Participant('Bob', {
    // Specify Bob's interact interface here
  });
  init();
  
  A.only(() => { const params = declassify(interact.getParams()) });
  A.publish(params);
  commit();

  B.publish();
  commit();

  A.publish();

  const { name, symbol, url, metadata, supply, decimals } = params; 
  const tok = new Token({ name, symbol, url, metadata, supply, decimals });
  transfer(supply, tok).to(B);

  var [] = [];
  invariant(balance() == 0 && balance(tok) >= 0);
  while(true) { 
    commit();
    A.only(() => { interact.doNothing() });
    A.publish();
    continue;
  }

  tok.burn();
  assert(tok.destroyed() == false);
  tok.destroy();

  commit();
  exit();
});
