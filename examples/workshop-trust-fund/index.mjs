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

  const Common = (who) => ({
    ready: async () => console.log(`${who} is ready to receive the funds.`),
    recvd: async () => console.log(`${who} received the funds.`),
  });

  const startingBalance = toCurrency('100');

  const funder = await stdlib.newTestAccount(startingBalance);
  const receiver = await stdlib.newTestAccount(startingBalance);
  const bystander = await stdlib.newTestAccount(startingBalance);

  const ctcFunder = await funder.deploy(backend);
  const ctcReceiver = await receiver.attach(backend, ctcFunder);
  const ctcBystander = await bystander.attach(backend, ctcFunder);

  const receiverP = (async () => {
    console.log('Receiver begins to wait...');
    await stdlib.wait(delay, ({currentTime, targetTime}) => {
      console.log(`Receiver wait progress... ${currentTime} -> ${targetTime}`);
    });
    await backend.Receiver(stdlib, ctcReceiver, Common('Receiver'));
  })();

  await Promise.all([
    backend.Funder(stdlib, ctcFunder, {
      ...Common('Funder'),
      funded: () => console.log(`Funder made the first payment`),
      getParams: () => ({
        receiverAddr: receiver.networkAccount.address,
        payment: toCurrency('10'),
        maturity: MATURITY,
        refund: REFUND,
        dormant: DORMANT,
      }), }),
    receiverP,
    backend.Bystander(stdlib, ctcBystander, Common('Bystander'))]);
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
