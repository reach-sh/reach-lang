import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

const MATURITY = 50;
const REFUND = 50;
const DORMANT = 50;

const runDemo = async (fdelay = 0, rdelay = 0) => {
  const stdlib = await stdlib_loader.loadStdlib();
  const connector = stdlib_loader.getConnector();

  const toCurrency =
        connector == 'ETH' ? (amt) => stdlib.toWeiBigNumber(amt, 'ether') :
        (amt) => stdlib.bigNumberify(amt);
  const fromCurrency =
        connector == 'ETH' ? (amt) => stdlib.fromWei(amt) :
        (amt) => amt.toString(); // ?
  const getBalance = async (who) => fromCurrency(await stdlib.balanceOf(who));

  console.log(`Begin demo with funder delay(${fdelay}) and receiver delay(${rdelay}).`);

  const common = (who, delay = 0) => ({
    funded: async () => {
      console.log(`${who} sees that the account is funded.`);
      if ( delay != 0 ) {
        console.log(`${who} begins to wait...`);
        await stdlib.wait(delay); } },
    ready : async () => console.log(`${who} is ready to receive the funds.`),
    recvd : async () => console.log(`${who} received the funds.`) });

  const startingBalance = toCurrency('100');

  const funder = await stdlib.newTestAccount(startingBalance);
  const receiver = await stdlib.newTestAccount(startingBalance);
  const bystander = await stdlib.newTestAccount(startingBalance);

  const ctcFunder = await funder.deploy(backend);
  const ctcReceiver = await receiver.attach(backend, ctcFunder);
  const ctcBystander = await bystander.attach(backend, ctcFunder);

  await Promise.all([
    backend.Funder(stdlib, ctcFunder, {
      ...common('Funder', fdelay),
      getParams: () => ({
        receiverAddr: receiver.networkAccount.address,
        payment: toCurrency('10'),
        maturity: MATURITY,
        refund: REFUND,
        dormant: DORMANT, }) }),
    backend.Receiver(stdlib, ctcReceiver, common('Receiver', rdelay)),
    backend.Bystander(stdlib, ctcBystander, common('Bystander')) ]);
  for (const [who, acc] of [['Funder', funder], ['Receiver', receiver], ['Bystander', bystander]]) {
    console.log(`${who} has a balance of ${await getBalance(acc)}`); }
  console.log(`\n`); };

(async () => {
  await runDemo(0, 0);
  await runDemo(0, MATURITY + REFUND + 1);
  await runDemo(MATURITY + REFUND + DORMANT + 1, MATURITY + REFUND + 1);
})();
