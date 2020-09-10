import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

const MATURITY = 50;
const REFUND = 50;
const DORMANT = 50;

const runDemo = async (delay) => {
  delay = delay || 0;
  console.log(`Begin demo with delay ${delay}`);

  const stdlib = await stdlib_loader.loadStdlib();
  const connector = stdlib_loader.getConnector();
  const money = connector;
  const time = 'blocks';

  const fromCurrency =
        connector == 'ETH' ? (amt) => stdlib.fromWei(amt) :
        (amt) => amt.toString(); // ?

  const toCurrency =
        connector == 'ETH' ? (amt) => stdlib.toWeiBigNumber(amt, 'ether') :
        (amt) => stdlib.bigNumberify(amt);

  const getBalance = async (who) => fromCurrency(await stdlib.balanceOf(who));

  const notify = async (who, what) => {
    const at = await stdlib.getNetworkTime();
    console.log(`@${at}: ${who} is notified: ${what}`);
  };
  const hasNotifications = (who) => ({
    notifyStarted: async () => {
      await notify(who, 'The program has started');
    },
    notifyWillReceive: async ({payment, maturity}) => {
      const paymentStr = `${fromCurrency(payment)} ${money}`;
      const maturityStr = `${maturity} ${time}`;
      await notify(
        who,
        `Receiver will receive ${paymentStr} after ${maturityStr}.`
      );
    },
    notifyMaturity: async ({refund}) => {
      const refundStr = `${refund} ${time}`;
      await notify(
        who,
        `The fund has reached maturity.`
          + ` Receiver has up to ${refundStr} to collect.`
      );
    },
    notifyReceived: async () => {
      await notify(who, 'Receiver has received the payment.');
    },
    notifyReceiverTookTooLong: async ({dormant}) => {
      const dormantStr = `${dormant} ${time}`;
      await notify(
        who,
        `Receiver took too long.`
          + ` Funder has up to ${dormantStr} to get refunded.`
      );
    },
    notifyRefunded: async () => {
      await notify(who, 'Funder has been refunded the payment.');
    },
    notifyFunderTookTooLong: async () => {
      await notify(
        who,
        'Funder took took too long'
          + ' Bystander may now yoink the abandoned payment.'
      );
    },
    notifyYoinked: async () => {
      await notify(who, 'Bystander has yoinked the abandoned payment.');
    },
    notifyExited: async () => {
      await notify(who, 'The program has exited');
    }
  });

  const startingBalance = toCurrency('100');

  const funder = await stdlib.newTestAccount(startingBalance);
  const receiver = await stdlib.newTestAccount(startingBalance);
  const bystander = await stdlib.newTestAccount(startingBalance);

  const ctcFunder = await funder.deploy(backend);
  const ctcReceiver = await receiver.attach(backend, ctcFunder);
  const ctcBystander = await bystander.attach(backend, ctcFunder);

  const params = {
    ReceiverAddress: receiver.networkAccount.address,
    payment: toCurrency('10'),
    maturity: MATURITY,
    refund: REFUND,
    dormant: DORMANT,
  };

  const funderP = (async () => {
    await backend.Funder(stdlib, ctcFunder, {
      ...hasNotifications('Funder'),
      getParams: () => params,
    });
  })();

  const receiverP = (async () => {
    console.log('Receiver begins to wait...');
    await stdlib.wait(delay, ({currentTime, targetTime}) => {
      console.log(`Receiver wait progress... ${currentTime} -> ${targetTime}`);
    });
    try {
      await backend.Receiver(stdlib, ctcReceiver, {
        ...hasNotifications('Receiver'),
      });
    } catch (e) {
      notify('Receiver', `Receiver errored: ${e.message}`);
    }
  })();

  const bystanderP = (async () => {
    await backend.Bystander(stdlib, ctcBystander, {
      ...hasNotifications('Bystander'),
    });
  })();

  await Promise.all([funderP, receiverP, bystanderP]);
  for (const [who, acc] of [['Funder', funder], ['Receiver', receiver], ['Bystander', bystander]]) {
    console.log(`${who} has a balance of ${await getBalance(acc)}`);
  }
  console.log(`demo complete with delay ${delay}`);
};

(async () => {
  await runDemo();
  await runDemo(MATURITY + REFUND + 20);
  // TODO: demo Bystander yoink
})();
