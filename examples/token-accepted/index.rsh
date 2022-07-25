'reach 0.1'

export const main = Reach.App(() => {
  const P = {
    seeTokenAccepted: Fun([Token, Bool], Null),
  };
  const Alice = Participant('Alice', {
    ...P,
  });
  const Bob = Participant('Bob', {
    ...P,
  });
  init();


  Alice.publish();
  const tok = new Token({supply: 10, decimals: 0});
  commit();

  Bob.publish();
  const x = Token.accepted(Bob, tok);
  commit();

  Bob.only (() => { const bobX = x; });

  Bob.publish(bobX);
  check(bobX == x);
  commit();

  Bob.only(() => {
    interact.seeTokenAccepted(tok, x);
  })

  Bob.publish();
  const y = Token.accepted(Bob, tok);
  commit();

  Bob.only (() => { const bobY = y; });

  Bob.publish(bobY);
  check(bobY == y);
  commit();

  Bob.only(() => {
    interact.seeTokenAccepted(tok, y);
  })

  Bob.publish();
  tok.burn(10);
  tok.destroy();
  commit();

})

