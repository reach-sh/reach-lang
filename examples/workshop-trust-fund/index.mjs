import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

const MATURITY = 10;
const REFUND = 5;
const DORMANT = 5;

const runDemo = async (delay) => {
  delay = delay || 0;
  console.log(`Begin demo with delay ${delay}`);

  const stdlib = await stdlib_loader.loadStdlib();
  const connector = stdlib_loader.getConnector();

  // stdlib.setDEBUG(true);

  const fromCurrency =
        connector == 'ETH' ? (amt) => stdlib.fromWei(amt) :
        (amt) => amt.toString(); // ?

  const toCurrency =
        connector == 'ETH' ? (amt) => stdlib.toWeiBigNumber(amt, 'ether') :
        (amt) => stdlib.bigNumberify(amt);

  const getBalance = async (who) => fromCurrency(await stdlib.balanceOf(who));

  const notify = (who, what) => {
    console.log(`${who} is notified that ${what}.`);
  };

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
    console.log(`Funder has joined the contract.`);
    await backend.Funder(stdlib, ctcFunder, {
      getParams: () => params,
      notifyRefunded: async () => {
        notify('Funder', 'Funder has been refunded the payment.');
      }
    });
    console.log(`Funder is done with the contract.`);
  })();

  const receiverP = (async () => {
    await stdlib.wait(delay, ({currentTime, targetTime}) => {
      console.log(`wait progress... ${currentTime} -> ${targetTime}`);
    });
    console.log(`Receiver joins the contract.`);
    try {
      await backend.Receiver(stdlib, ctcReceiver, {
        notifyWillReceive: ({payment, maturity}) => {
          const paymentStr = fromCurrency(payment);
          notify('Receiver', `Receiver will receive ${paymentStr} after ${maturity}.`);
        },
        notifyReceived: () => {
          notify('Receiver', 'Receiver has received the payment.');
        },
      });
    } catch (e) {
      notify('Receiver', `Receiver errored: ${e.message}`);
    }
    console.log(`Receiver is done with the contract.`);
  })();

  const bystanderP = (async () => {
    console.log(`Bystander joins the contract.`);
    await backend.Bystander(stdlib, ctcBystander, {});
    console.log(`Bystander is done with the contract.`);
  })();

  await Promise.all([funderP, receiverP, bystanderP]);
  console.log(`demo complete with delay ${delay}`);
};

(async () => {
  await runDemo();
  // TODO
  // await runDemo(20);
})();
