'reach 0.1 exe';

const Borrower = newParticipant();
const Lender = newParticipant();

const DELAY = 10; // in blocks

function main() {
  Borrower.only(() => {
    const [ _collateral,
            _pre,
            _post,
            _maturation ] = declassify(is([ uint256,
                                            uint256,
                                            uint256,
                                            uint256 ],
                                          interact.getParams()));
    assume(pre < post); });
  Borrower.publish(collateral, pre, post, maturation)
    .pay(collateral);
  require(pre < post);
  commit();

  Lender.pay(pre)
    .timeout(DELAY, closeTo(Borrower, false));
  transfer(pre).to(Borrower);
  commit();

  Borrower.only(() => {
    interact.waitForPayback(); });
  Borrower.pay(post)
    .timeout(maturation, closeTo(Lender, false));
  transfer(post).to(Lender);
  transfer(collateral).to(Borrower);
  commit();

  return true; }
