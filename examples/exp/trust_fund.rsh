'reach 0.1 exe';

const Payer = participant({ _payment: uint256,
                            _Recipient: address,
                            _maturity: uint256,
                            _refund: uint256,
                            _dormant: uint256
                          });
const Recipient = participant({});
const Bystander = participant({});

const DELAY = 10;

function main() {
  Payer.only(() => {
    const payment = declassify(_payment);
    const Recipient = declassify(_Recipient);
    const maturity = declassify(_maturity);
    const refund = declassify(_refund);
    const dormant = declassify(_dormant);
  });
  Payer.publish(payment, Recipient, maturity, refund, dormant)
    .pay(payment)
    .timeout(DELAY, _, () => {
      commit();
      return "Never funded";
    });
  commit();

  Recipient.publish()
    .wait(maturity)
    .timeout(refund, Payer, () => {
      transfer(payment).to(Payer);
      commit();
      return "Returned to funder"; })
    .timeout(dormant, Bystander, () => {
      transfer(payment).to(Bystander);
      commit();
      return "Removed by bystander"; });
  transfer(payment).to(Recipient);
  commit();

  return "Extracted by recepient";
};
