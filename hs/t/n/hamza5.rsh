'reach 0.1'; 

const TokenParams = Object({
  name: Bytes(32), 
  symbol: Bytes(8), 
  url: Bytes(96), 
  metadata: Bytes(32), 
  supply: UInt, 
  decimals: UInt
});

export const main = Reach.App(() => {
  const Alice = Participant("Alice", {
    halt: Fun([], Null),
    tokenParams: TokenParams
  });
  init();
  
  Alice.only(() => {
    const { name, symbol, url, metadata, supply, decimals } = 
      declassify(interact.tokenParams);
  });
  Alice.publish(name, symbol, url, metadata, supply, decimals);

  const token = new Token({ name, symbol, url, metadata, supply, decimals });
  transfer(supply, token).to(Alice);

  // preserveToken(Alice, token, supply);
  var [] = [];
  invariant(balance() == 0 && balance(token) <= supply && !token.destroyed());
  while(balance(token) < supply) { 
    commit();
    Alice.only(() => { interact.halt() });
    Alice.publish();
    continue;
  }
  assert(balance(token) == supply);

  token.burn(balance(token));
  
  assert(!token.destroyed());
  token.destroy();
  
  commit();
  exit();
});
