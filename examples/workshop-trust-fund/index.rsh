'reach 0.1';

const common = {
  funded: Fun([], Null),
  ready : Fun([], Null),
  recvd : Fun([UInt256], Null) };

export const main =
  Reach.App(
    { deployMode: 'firstMsg' },
    [ [   'Funder', {
      ...common,
      getParams: Fun([], Object({
        receiverAddr: Address,
        payment:      UInt256,
        maturity:     UInt256,
        refund:       UInt256,
        dormant:      UInt256 })) }],
      [ 'Receiver', common],
      ['Bystander', common] ],
    (Funder, Receiver, Bystander) => {
      Funder.only(() => {
        const { receiverAddr,
                payment, maturity, refund, dormant }
              = declassify(interact.getParams()); });
      Funder.publish(
        receiverAddr,
        payment, maturity, refund, dormant )
        .pay(payment);
      Receiver.set(receiverAddr);
      commit();

      each([Funder, Receiver, Bystander], () => {
        interact.funded(); });
      wait(maturity);

      const payTo = (Who) => {
        transfer(payment).to(Who);
        commit();
        Who.only(() => interact.recvd(payment));
        exit(); };

      Receiver.only(() => interact.ready());
      Receiver.publish()
        .timeout(refund, () => {
          Funder.only(() => interact.ready());
          Funder.publish()
            .timeout(dormant, () => {
              Bystander.only(() => interact.ready());
              Bystander.publish();
              payTo(Bystander); });

          payTo(Funder); });

      payTo(Receiver); } );
