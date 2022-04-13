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
  const LA = API('LoanAPI', {
    payBack: Fun([], Null),
    takeCollateral: Fun([], Null),
  });
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

  try {
    const [[], k] = call(LA.payBack)
      .pay(() => loanAmt + interest)
      .throwTimeout(relativeTime(deadline), null);
    k(null);
    transfer(collateral).to(B);
    transfer(loanAmt + interest).to(L);
  } catch (_) {
    // B timed out on paying back the loan
    const [[], k] = call(LA.takeCollateral);
    k(null);
    transfer(collateral).to(L);
  }

  commit();
  exit();
});
