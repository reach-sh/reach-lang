'reach 0.1';

const Common = {
  ready: Fun([], Null),
  recvd: Fun([UInt256], Null),
};

export const main =
  Reach.App(
    { deployMode: 'firstMsg' },
    [
      ['Funder', {
        ...Common,
        funded: Fun([], Null),
        getParams: Fun([], Object({
          receiverAddr:     Address,
          payment:          UInt256, // WEI
          maturity:         UInt256, // time delta
          refund:           UInt256, // time delta
          dormant:          UInt256, // time delta
        })),
      }],
      ['Receiver', {
        ...Common,
      }],
      ['Bystander', {
        ...Common,
      }],
    ],
    (Funder, Receiver, Bystander) => {
      Funder.only(() => {
        const {
          receiverAddr,
          payment, maturity, refund, dormant,
        } = declassify(interact.getParams());
      });
      Funder.publish(
        receiverAddr,
        payment, maturity, refund, dormant
      ).pay(payment);
      Receiver.set(receiverAddr);
      commit();

      Funder.only(() => {
        interact.funded(); });
      wait(maturity);

      /*
giveChance(
        Receiver,
        { len: refund,
          after:
          () =>
          giveChance(
            Funder,
            { len: dormant,
              after:
              giveChance(
                Bystander,
                false ) }) } );
      */
      
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
