'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const B = Participant('Borrower', {
    loanAmt: UInt,
    interest: UInt,
    nft: Token,    
    deadline: UInt,
    loanMade: Fun([], Null),
  });
  const L = Participant('Lender', {});
  init();
  
  B.only(() => {
    const [loanAmt, interest, nft, deadline] = declassify([
      interact.loanAmt, interact.interest, interact.nft, interact.deadline
    ]);
  })
  B.publish(loanAmt, interest, nft, deadline);
  commit();

  const collateral = [[1, nft]];
  
  B.pay(collateral);
  commit();

  L.pay(loanAmt);
  transfer(loanAmt).to(B);
  commit();
  
  B.interact.loanMade();
  B.pay(loanAmt + interest)
   .timeout(relativeTime(deadline), () => {
      L.publish();
      transfer(collateral).to(L);
      commit();
      exit();
    });
  transfer(collateral).to(B);
  transfer(loanAmt + interest).to(L);
  commit();
  exit();
});
