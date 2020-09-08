'reach 0.1';

const HasNotifications = {
  notifyStarted: Fun([], Null),
  notifyWillReceive: Fun([Object({
    payment:  UInt256, // WEI
    maturity: UInt256, // time delta
  })], Null),
  notifyMaturity: Fun([Object({
    refund:   UInt256, // time delta
  })], Null),
  notifyReceived: Fun([], Null),
  notifyReceiverTookTooLong: Fun([Object({
    dormant:  UInt256, // time delta
  })], Null),
  notifyRefunded: Fun([], Null),
  notifyFunderTookTooLong: Fun([], Null),
  notifyYoinked: Fun([], Null),
  notifyExited: Fun([], Null),
};

const FunderInteract = {
  ...HasNotifications,
  getParams: Fun([], Object({
    ReceiverAddress:  Address,
    payment:          UInt256, // WEI
    maturity:         UInt256, // time delta
    refund:           UInt256, // time delta
    dormant:          UInt256, // time delta
  })),
};

const ReceiverInteract = {
  ...HasNotifications,
};

const BystanderInteract = {
  ...HasNotifications,
};

const main_fn = (Funder, Receiver, Bystander) => {
  const everyone = (interaction) => {
    each([Funder, Receiver, Bystander], () => {
      interaction(interact);
    });
  };
  everyone((interact) => interact.notifyStarted());

  Funder.only(() => {
    const {
      ReceiverAddress,
      payment, maturity, refund, dormant,
    } = declassify(interact.getParams());
  });
  Funder.publish(
    ReceiverAddress,
    payment, maturity, refund, dormant
  ).pay(payment);
  Receiver.set(ReceiverAddress);
  commit();

  everyone((interact) => interact.notifyWillReceive({payment, maturity}));

  wait(maturity);
  everyone((interact) => interact.notifyMaturity({refund}));

  const doExit = () => {
    everyone((interact) => {
      interact.notifyExited();
    });
    exit();
  };

  const doYoink = () => {
    everyone((interact) => {
      interact.notifyFunderTookTooLong();
    });
    Bystander.publish();
    transfer(payment).to(Bystander);
    commit();
    everyone((interact) => interact.notifyYoinked());
    doExit();
  };

  const doRefund = () => {
    everyone((interact) => {
      interact.notifyReceiverTookTooLong({dormant});
    });
    Funder.publish()
      .timeout(dormant, () => doYoink());
    transfer(payment).to(Funder);
    commit();
    everyone((interact) => interact.notifyRefunded());
    doExit();
  };

  Receiver.publish()
    .timeout(refund, () => doRefund());
  transfer(payment).to(Receiver);
  commit();

  everyone((interact) => interact.notifyReceived());

  doExit();
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
