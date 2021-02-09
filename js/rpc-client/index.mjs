import waitPort from 'wait-port';
import bent from 'bent';

const rpc_server = process.env.REACH_RPC_SERVER;
const rpc_port = process.env.REACH_RPC_PORT;
const call = bent(`https://${rpc_server}:${rpc_port}`, `POST`, `json`, 200, {
  'X-API-Key': process.env.REACH_RPC_KEY,
});

export const rpcReady = async () => {
  await waitPort({host: rpc_server, port: parseInt(rpc_port)}); };

export const rpc = async (m, ...args) => {
  const lab = `RPC ${m} ${JSON.stringify(args)}`
  console.log(`${lab}`);
  const ans = await call(m, args);
  console.log(`${lab} ==> ${JSON.stringify(ans)}`);
  return ans;
};

export const rpcCallbacks = async (m, arg, cbacks) => {
  const vals = {};
  const meths = {};
  for (const k in cbacks) {
    const v = cbacks[k];
    if ( v instanceof Function ) {
      meths[k] = true;
    } else {
      vals[k] = v;
    }
  }
  return new Promise((resolve, reject) => (async () => {
    let p = rpc(m, arg, vals, meths);
    while (true) {
      try {
        const r = await p;
        switch ( r.t ) {
          case 'Done': {
            return resolve(r.ans);
          }
          case 'Kont': {
            const { kid, m, args } = r;
            const ans = await cbacks[m](...args);
            p = rpc(`/kont`, kid, ans);
            break;
          }
          default:
            throw new Error(`Illegal callback return: ${JSON.stringify(r)}`);
        }
      } catch (e) {
        return reject(e);
      }
    }
  })());
};
