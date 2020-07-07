'reach 0.1 exe';

const Borrower = participant({_collateral: uint256,
                              _pre: uint256,
                              _post: uint256,
                              _maturation: uint256 });
const Lender = participant({});

const DELAY = 10; // in blocks

function main() {
  Borrower.only(() => {
    const collateral = declassify(_collateral);
    const pre = declassify(_pre);
    const post = declassify(_post);
    assume(pre < post);
    const maturation = declassify(_maturation); });
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
