'reach 0.1 exe';

const Payer = newParticipant();
const Recipient = newParticipant();
const Bystander = newParticipant();

const DELAY = 10;

function main() {
  Payer.only(() => {
    const [ _payment,
            _Recipient,
            _maturity,
            _refund,
            _dormant
          ] = declassify(is([ uint256,
                              address,
                              uint256,
                              uint256,
                              uint256
                            ], interact.getParams())); });
  Payer.publish(payment, Recipient, maturity, refund, dormant)
    .pay(payment);
  commit();

  Recipient.publish()
    .wait(maturity)
    .timeout(refund, closeTo(Payer, "Returned to funder"))
    .timeout(dormant, closeTo(Bystander, "Removed by bystander"));
  transfer(payment).to(Recipient);
  commit();

  return "Extracted by recepient";
};
