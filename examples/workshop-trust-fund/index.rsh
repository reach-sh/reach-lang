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

      const giveChance = (Who, then) => {
        Who.only(() => interact.ready());

        if ( then ) {
          Who.publish()
            .timeout(then.deadline, () => then.after()); }
        else {
          Who.publish(); }

        transfer(payment).to(Who);
        commit();
        Who.only(() => interact.recvd(payment));
        exit(); };

      giveChance(
        Receiver,
        { deadline: refund,
          after: () =>
          giveChance(
            Funder,
            { deadline: dormant,
              after: () =>
              giveChance(Bystander, false) }) }); } );
