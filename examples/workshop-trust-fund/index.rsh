'reach 0.1';

const FunderInteract = {
  getParams: Fun([], Object({
    ReceiverAddress:  Address,
    BystanderAddress: Address,
    payment:          UInt256, // WEI
    maturity:         UInt256, // time delta
    refund:           UInt256, // time delta
    dormant:          UInt256, // time delta
  })),
  notifyRefunded: Fun([], Null),
};

const ReceiverInteract = {
  notifyWillReceive: Fun([Object({
    payment:  UInt256, // WEI
    maturity: UInt256, // time delta
  })], Null),
  notifyReceived: Fun([], Null),
};

const BystanderInteract = {};

const main_fn = (Funder, Receiver, Bystander) => {
  Funder.only(() => {
    const {
      ReceiverAddress, BystanderAddress,
      payment, maturity, refund, dormant,
    } = declassify(interact.getParams());
  });
  Funder.publish(
    ReceiverAddress, BystanderAddress,
    payment, maturity, refund, dormant
  ).pay(payment);
  Receiver.set(ReceiverAddress);
  commit();

  Receiver.only(() => {
    interact.notifyWillReceive({payment, maturity});
  });

  wait(maturity);

  const doRefund = () => {
    Funder.publish()
      .timeout(dormant, () => closeTo(Bystander, () => {}));
    transfer(payment).to(Funder);
    commit();
    Funder.only(() => {
      interact.notifyRefunded();
    });
    exit(); // you NEED this exit why?
  };

  Receiver.publish()
    .timeout(refund, () => doRefund());
  transfer(payment).to(Receiver);
  commit();

  Receiver.only(() => {
    interact.notifyReceived();
  });

  exit();
};

export const main = Reach.App(
  {},
  [
    ['Funder', FunderInteract],
    ['Receiver', ReceiverInteract],
    ['Bystander', BystanderInteract],
  ],
  (F, R, B) => main_fn(F, R, B)
);
