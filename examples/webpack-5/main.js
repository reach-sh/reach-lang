const reachsdk = require('@reach-sh/stdlib');
const process = require('process');

const out = [];
reachsdk.unsafeAllowMultipleStdlibs();
for (const connector of ['ETH', 'ALGO', 'CFX']) {
  const reach = reachsdk.loadStdlib({REACH_CONNECTOR_MODE: connector});
  reach.assert(connector === reach.connector, 'Connector string mismatch');
  out.push(`${connector} === ${reach.connector}; // true`);
  out.push(reach.randomUInt().toString());
}
console.log(out.join('\n'));
if (process.browser) { document.write(out.join('<br />')); }
