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

const [ isOutcome, LENDER_TIMEOUT, BORROWER_TIMEOUT ] = makeEnum(2);
const hasSendOutcome = {
  sendOutcome: Fun([UInt], Null),
};

export const main = Reach.App(() => {
  const Borrower = Participant('Borrower', {
    ...hasSendOutcome,
    getParams: Fun([], ParamsType),
    waitForPayback: Fun([], Null),
  });
  const Lender = Participant('Lender', {
    ...hasSendOutcome,
    acceptParams: Fun([ParamsType], Null),
  });

  init();

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
  Borrower.publish(params, tok);
  const { collateral, pre, post, maturation, maxLenderDelay }
        = params;
  require(pre < post);
  commit();
  Borrower.pay([ [params.collateral, tok] ]);
  commit();

  Lender.only(() => {
    interact.acceptParams(params);
  });
  Lender.pay(pre)
    .timeout(relativeTime(maxLenderDelay), () =>
      closeTo(Borrower,
        sendOutcome(LENDER_TIMEOUT),
        [[balance(tok), tok]]));
  transfer(pre).to(Borrower);
  commit();

  Borrower.only(() => {
    interact.waitForPayback();
  });
  Borrower.pay(post)
    .timeout(relativeTime(maturation), () =>
      closeTo(Lender,
        sendOutcome(BORROWER_TIMEOUT),
        [[balance(tok), tok]]));
  transfer(post).to(Lender);
  transfer([ [collateral, tok] ]).to(Borrower);
  commit();
});
