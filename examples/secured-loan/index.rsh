'reach 0.1';
'use strict';

const ParamsType = Object({
  collateral: UInt,
  pre: UInt,
  post: UInt,
  maturation: UInt,
  maxLenderDelay: UInt,
  tok: Token
});

const hasSendOutcome = {
  sendOutcome: Fun([Bytes(128)], Null),
};

export const main = Reach.App(
  {},
  [
    Participant('Borrower', {
      ...hasSendOutcome,
      getParams: Fun([], ParamsType),
      waitForPayback: Fun([], Null),
    }),
    Participant('Lender', {
      ...hasSendOutcome,
      acceptParams: Fun([ParamsType], Null),
    }),
  ],
  (Borrower, Lender) => {
    const sendOutcome = (which) => {
      return (() => {
        each([Borrower, Lender], () => {
          interact.sendOutcome(which);
        });
      });
    };

    Borrower.only(() => {
      const params = declassify(interact.getParams());
      const tok = params.tok;
      assume(params.pre < params.post); });
    Borrower.publish(params, tok)
      .pay([ [params.collateral, tok] ]);
    const { collateral, pre, post, maturation, maxLenderDelay }
          = params;
    require(pre < post);
    commit();

    Lender.only(() => {
      interact.acceptParams(params);
    });
    Lender.pay(pre)
      .timeout(maxLenderDelay, () =>
        closeTo(Borrower,
          sendOutcome('Lender failed to lend on time'),
          [[balance(tok), tok]]));
    transfer(pre).to(Borrower);
    commit();

    Borrower.only(() => {
      interact.waitForPayback();
    });
    Borrower.pay(post)
      .timeout(maturation, () =>
        closeTo(Lender,
          sendOutcome('Borrower failed to pay on time'),
          [[balance(tok), tok]]));
    transfer(post).to(Lender);
    transfer([ [collateral, tok] ]).to(Borrower);
    commit();
  }
);
