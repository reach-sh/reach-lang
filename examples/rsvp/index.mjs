import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib({ REACH_NO_WARN: 'Y' });
const sbal = stdlib.parseCurrency(100);
const accD = await stdlib.newTestAccount(sbal);

const deadline = stdlib.connector === 'CFX' ? 500 : 250;

const ctcD = accD.contract(backend);

await stdlib.withDisconnect(() =>
  ctcD.p.Admin({
    deadline,
    price: stdlib.parseCurrency(25),
    ready: stdlib.disconnect
  })
);

const users = await stdlib.newTestAccounts(5, sbal);

const willError = async (f) => {
  let e;
  try {
    await f();
    e = false;
  } catch (te) {
    e = te;
  }
  if ( e === false ) {
    throw Error(`Expected to error, but didn't`);
  }
  console.log(`That last call errored and that's okay`);
};
const ctcWho = (whoi) =>
  users[whoi].contract(backend, ctcD.getInfo());
const rsvp = async (whoi) => {
  const who = users[whoi];
  const ctc = ctcWho(whoi);
  console.log('RSVP of', stdlib.formatAddress(who));
  await ctc.apis.Attendee.iWillGo();
};
const do_checkin = async (ctc, whoi) => {
  const who = users[whoi];
  console.log('Check in of', stdlib.formatAddress(who));
  await ctc.apis.Checkin.theyCame(who);
};
const checkin = (whoi) => do_checkin(ctcD, whoi);
const bad_checkin = (whoi) => do_checkin(ctcWho(whoi), whoi);
const timesup = async () => {
  console.log('I think time is up');
  await ctcD.apis.Checkin.timesUp();
};

await rsvp(0);
await rsvp(1);
await rsvp(2);
await rsvp(4);
await willError(() => rsvp(4));
await checkin(4);
await checkin(0);
await willError(() => checkin(3));
await willError(() => bad_checkin(1));
await checkin(2);
await willError(() => checkin(2));

console.log(`We're gonna wait for the deadline`);
await stdlib.wait(deadline);

await timesup();

for ( const who of [ accD, ...users ]) {
  console.warn(stdlib.formatAddress(who), 'has',
    stdlib.formatCurrency(await stdlib.balanceOf(who)));
}
