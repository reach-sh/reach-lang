'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    params: Object({
      min: UInt,
      tok: Token,
      supply: UInt,
    }),
    launched: Fun([Contract], Null),
  });
  const B = API('Buyer', {
    purchase: Fun([UInt], Null),
    refund: Fun([], UInt),
  });
  init();
 
  A.only(() => {
   const {min, tok, supply} = declassify(interact.params);
  });
  A.publish(min, tok, supply);
  commit();
  A.pay([[supply, tok]])
  A.interact.launched(getContract());

  const pMap = new Map(UInt);
  const [tokensSold, total] = parallelReduce([0, 0])
    .paySpec([tok])
    .invariant(pMap.sum() == total, "tracking total wrong")
    .invariant(balance() == total, "network token balance wrong")
    .invariant(balance(tok) == supply - tokensSold, "non-network token balance wrong")
    .while(tokensSold < supply)
    .api_(B.purchase, (amount) => {
      check(tokensSold != supply, "sorry, out of tickets");
      check(isNone(pMap[this]), "sorry, you are already in this list");
      check(amount >= min, "sorry, amount too low");
      return[[amount, [0, tok]], (ret) => {
        pMap[this] = amount;
        transfer(1, tok).to(this);
        ret(null);
        return [tokensSold + 1, total + amount];
      }];
    })
    .api_(B.refund, () => {
      check(isSome(pMap[this]), "sorry, you are not in the list");
      return[[0, [1, tok]], (ret) => {
        const paid = fromSome(pMap[this], 0);
        transfer(paid).to(this);
        ret(paid);
        delete pMap[this];
        return[tokensSold - 1, total - paid]
      }];
    });
  transfer(total).to(A);
  commit();
  exit();
});

