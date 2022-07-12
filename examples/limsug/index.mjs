import * as bin from './build/index.main.mjs';
import * as reachsdk from '@reach-sh/stdlib';
const stdlib = reachsdk.loadStdlib();
const es = [];
stdlib.setCustomHttpEventHandler((e) => {
  es.push(e);
});
const amt = stdlib.parseCurrency('100');
const [ driver, outsider ] = await stdlib.newTestAccounts(2, amt);
const ctcD = driver.contract(bin);
const ctcO = outsider.contract(bin, ctcD.getInfo());
await stdlib.withDisconnect(async () => {
  await ctcD.p.Driver({i: 42, ready: stdlib.disconnect});
});
const before = es.length;
console.log('before', before);
const info = await ctcO.v.Info();
const after = es.length;
console.log('after', after);
const reqs = (after - before) / 2
console.log(`${reqs} requests sent for view`);
const viewEvents = es.slice(before);
const lab = (e) => `${e.eventName} ${e.label}: ${e.method} ${e.relativePath}`;
const algo = stdlib.connector === 'ALGO';
let slide = 0;
let seen = false;
const sizes = [];
for (const e of viewEvents) {
  if (e.eventName !== 'before') {
    const body = JSON.parse(e.response.body) || {};
    const txns = body.transactions;
    if (algo && txns) {
      if (e.eventName !== 'success') { throw Error(`Expected only successful http responses for view on Algorand`); }
      if (seen) { throw Error(`Expected only one /v2/transactions to be seen for view on Algorand, saw more.`); }
      if (txns.length > 1) { throw Error(`Expected /v2/transactions result to be a single txn on Algorand, saw more.`); }
      if (txns.length === 0) {
        // We ignore the queries that find nothing; this is an artifact of the testing setup
        slide++;
        continue;
      }
      if (e.relativePath === '/v2/transactions') {
        seen = true;
      }
      console.log(e.response.body);
      console.log(lab(e), e.query);
      console.log(`${txns.length} txns in response`);
    }
    // excludes those that hit "continue" above
    sizes.push({path: e.relativePath, size: e.response.body.length});
  }
}
if (algo && !seen) { throw Error(`Expected /v2/transactions to be called by view on Algorand but saw none`); }
console.log(`${reqs - slide} real reqs sent for view`);
console.table(sizes);
const totalSize = sizes.reduce((acc, r) => acc + r.size, 0);
console.log(`sum of response sizes: ${totalSize} bytes`);
await ctcO.a.halt();
