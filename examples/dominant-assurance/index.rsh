'reach 0.1';
'use strict';

/* Based off of https://en.wikipedia.org/wiki/Assurance_contract#Dominant_assurance_contracts
   Please read that short Wikipedia article to understand the meaning behind participant names
   and the fields of Product's interact object. */

const InvestmentStructureT = Object({
  entrepreneurInvestment: UInt, // Entrepreneur's contribution to the product funding
  entrepreneurProfit: UInt,     // Entrepreneur's profit when the quorum is met
  investorInvestment: UInt,     // Each investor's contribution to the product funding
  investorFailProfit: UInt,     // Each investor's profit if the quorum is not met
  investorQuorum: UInt,         // Number of investors needed to successfully fund the product
  investmentDuration: UInt,     // How long funding will be open to investors
  failPayDuration: UInt,        // How long failure pay will be available for investors to claim
});

export const main = Reach.App(() => {
  setOptions({ ALGOExitMode: 'DeleteAndCloseOutASAs' });
  const P = Participant('Product', {
    investmentStructure: InvestmentStructureT,
    ready: Fun([], Null),
  });
  const E = Participant('Entrepreneur', {});
  const I = API('Investor', {
    invest: Fun([], Null),
    collectFailPay: Fun([], Null),
  });
  const PA = API('ProductAPI', {
    startInvestment: Fun([], Null),
    investmentTimeout: Fun([], Null),
    failPayTimeout: Fun([], Null),
  });
  const Phase = Data({ Investment: Null, FailPay: Null, Finished: Null });
  const CP = Events('ContractPhase', { phase: [Phase] });
  init();

  const checkInvestmentStructure = (iso) => {
    const expectedFunds = iso.entrepreneurInvestment
                        + iso.investorQuorum * iso.investorInvestment;
    check(iso.entrepreneurProfit <= expectedFunds);
    check(iso.investorQuorum > 1);
    check(iso.investorInvestment > 0);
    check(iso.investorFailProfit > 0);
  };

  // A participant representing the product being funded specifies how investment will work
  P.only(() => {
    const investmentStructure = declassify(interact.investmentStructure);
    checkInvestmentStructure(investmentStructure);
  });
  P.publish(investmentStructure);
  checkInvestmentStructure(investmentStructure);

  const {
    entrepreneurInvestment,
    entrepreneurProfit,
    investorInvestment,
    investorQuorum,
    investorFailProfit,
    investmentDuration,
    failPayDuration
  } = investmentStructure;

  commit();

  // Entrepreneur kicks off the contract by paying their investment plus enough
  // to compensate investors in the case of failure
  const starterInvestment = entrepreneurInvestment
                          + investorFailProfit * (investorQuorum - 1);
  E.pay(starterInvestment);
  commit();

  P.interact.ready();

  const awaitProductApi = (apiFunc) => {
    const [[], k] = call(apiFunc).assume(() => check(this == P));
    check(this == P);
    k(null);
  }

  awaitProductApi(PA.startInvestment);
  CP.phase(Phase.Investment());

  // In a real-world application, this would probably be absolute
  // Using relativeTime is easier for testing
  const investmentTimeout = relativeTime(investmentDuration);
  const investors = new Set();

  // Investors are given a change to invest
  const [timedOut, numInvestors] =
    parallelReduce([false, 0])
    .invariant(balance() == starterInvestment + numInvestors * investorInvestment)
    .invariant(investors.Map.size() == numInvestors)
    .invariant(numInvestors <= investorQuorum)
    .while(!timedOut && numInvestors < investorQuorum)
    .api_(I.invest, () => {
      check(!investors.member(this));
      return [ investorInvestment, (k) => {
        investors.insert(this);
        k(null);
        return [false, numInvestors + 1];
      }];
    })
    .timeout(investmentTimeout, () => {
      awaitProductApi(PA.investmentTimeout);
      return [true, numInvestors];
    });

  if (numInvestors == investorQuorum) {
    // Funding succeeded
    // The entrepreneur is paid their entrepreneurship incentive profit,
    // and the remainder of funds are sent to the product.
    transfer(entrepreneurProfit).to(E);
  } else {
    // Funding failed
    // The entrepreneur must be returned their starter investment plus unnecessary fail pay,
    // each investor must be given the opportunity to claim their fail pay,
    // and any unclaimed fail pay will be given to the product.
    const returnedToEntrepreneur = starterInvestment
                                 - investorFailProfit * numInvestors;
    transfer(returnedToEntrepreneur).to(E);

    CP.phase(Phase.FailPay());

    // In a real-world application, this would probably be absolute
    // Using relativeTime is easier for testing
    const failPayTimeout = relativeTime(failPayDuration);
    const investorFailPay = investorInvestment + investorFailProfit

    const [timedOut_, unpaidInvestors] =
      parallelReduce([false, numInvestors])
      .while(!timedOut_ && unpaidInvestors > 0)
      .invariant(balance() == unpaidInvestors * investorFailPay)
      .api_(I.collectFailPay, () => {
        check(investors.member(this));
        return [ (k) => {
          investors.remove(this);
          transfer(investorFailPay).to(this);
          k(null);
          return [false, unpaidInvestors - 1];
        }];
      })
      .timeout(failPayTimeout, () => {
        awaitProductApi(PA.failPayTimeout);
        return [true, unpaidInvestors];
      });
  }

  transfer(balance()).to(P);
  CP.phase(Phase.Finished());
  commit();
  exit();
});
