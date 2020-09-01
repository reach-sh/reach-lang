'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Funder',
      { getParams:
        Fun([], Object({ payment: UInt256,
                         R_addr: Address,
                         maturity: UInt256,
                         refund: UInt256,
                         dormant: UInt256 })),
        refunded: Fun([], Null) }],
     ['Receiver',
      { willReceive: Fun([UInt256, UInt256], Null),
        received: Fun([], Null) }],
     ['Bystander', {}]],
    (F, R, B) => {
      F.only(() => {
        const { payment, R_addr, maturity,
                refund, dormant } =
              declassify(interact.getParams()); });
      F.publish(payment, R_addr, maturity,
                refund, dormant)
        .pay(payment);
      R.set(R_addr);
      commit();

      R.only(() => {
        interact.willReceive(payment, maturity); });

      wait(maturity);

      R.publish()
        .timeout(refund, () => {
          F.publish()
            .timeout(dormant, () => closeTo(B, () => {}));
          transfer(payment).to(F);
          commit();

          F.only(() => {
            interact.refunded(); });

          exit(); });
      transfer(payment).to(R);
      commit();

      R.only(() => {
        interact.received(); });

      exit(); });
