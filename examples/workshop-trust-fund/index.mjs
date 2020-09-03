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
    BystanderAddress: bystander.networkAccount.address,
    payment: toCurrency('10'),
    maturity: MATURITY,
    refund: REFUND,
    dormant: DORMANT,
  };

  const funderP = backend.Funder(stdlib, ctcFunder, {
    getParams: () => params,
    notifyRefunded: async () => {
      notify('Funder', 'Funder has been refunded the payment.');
    }
  });

  const receiverP = (async () => {
    await stdlib.wait(delay, () => console.log('wait progress...'));
    return await backend.Receiver(stdlib, ctcReceiver, {
      notifyWillReceive: ({payment, maturity}) => {
        const paymentStr = fromCurrency(payment);
        notify('Receiver', `Receiver will receive ${paymentStr} after ${maturity}.`);
      },
      notifyReceived: () => {
        notify('Receiver', 'Receiver has received the payment.');
      },
    });
  })();

  const bystanderP = backend.Bystander(stdlib, ctcBystander, {});

  await Promise.all([funderP, receiverP, bystanderP]);
  console.log(`demo complete with delay ${delay}`);
};

(async () => {
  await runDemo();
})();
