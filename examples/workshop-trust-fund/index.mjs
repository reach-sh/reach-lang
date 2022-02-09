import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const runDemo = async (delayReceiver, delayFunder) => {
  const stdlib = await loadStdlib();
  const getBalance = async (who) => stdlib.formatCurrency(
    await stdlib.balanceOf(who), 4,
  );

  const MATURITY = 10;
  const REFUND = 10;
  const DORMANT = 10;
  const fDelay = delayFunder ? MATURITY + REFUND + DORMANT + 1 : 0;
  const rDelay = delayReceiver ? MATURITY + REFUND + 1 : 0;
  console.log(`Begin demo with funder delay(${fDelay}) and receiver delay(${rDelay}).`);

  const common = (who, delay = 0) => ({
    funded: async () => {
      console.log(`${who} sees that the account is funded.`);
      if ( delay != 0 ) {
        console.log(`${who} begins to wait...`);
        await stdlib.wait(delay);
      }
    },
    ready : async () => console.log(`${who} is ready to receive the funds.`),
    recvd : async () => console.log(`${who} received the funds.`)
  });

  const startingBalance = stdlib.parseCurrency(100);

  const funder = await stdlib.newTestAccount(startingBalance);
  const receiver = await stdlib.newTestAccount(startingBalance);
  const bystander = await stdlib.newTestAccount(startingBalance);

  const ctcFunder = funder.contract(backend);
  const ctcReceiver = receiver.contract(backend, ctcFunder.getInfo());
  const ctcBystander = bystander.contract(backend, ctcFunder.getInfo());

  await Promise.all([
    backend.Funder(ctcFunder, {
      ...common('Funder', fDelay),
      getParams: () => ({
        receiverAddr: receiver.networkAccount,
        payment: stdlib.parseCurrency(10),
        maturity: MATURITY,
        refund: REFUND,
        dormant: DORMANT,
      }),
    }),
    backend.Receiver(ctcReceiver, common('Receiver', rDelay)),
    backend.Bystander(ctcBystander, common('Bystander')),
  ]);
  for (const [who, acc] of [['Funder', funder], ['Receiver', receiver], ['Bystander', bystander]]) {
    console.log(`${who} has a balance of ${await getBalance(acc)}`);
  }
  console.log(`\n`);
};

await runDemo(false, false);
await runDemo(true, false);
await runDemo(true, true);
