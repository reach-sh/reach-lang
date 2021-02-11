import waitPort from 'wait-port';
import bent     from 'bent';

export const mkRPC = opts => {
  const defaults = {
    host:   'REACH_RPC_SERVER',
    port:   'REACH_RPC_PORT',
    apiKey: 'REACH_RPC_KEY',
  };

  const r = (acc, k) =>
    Object.assign(acc, { [k]: process.env[defaults[k]] });

  const o = Object.assign(
    Object.keys(defaults).reduce(r, {}),
    opts);

  Object.keys(o).forEach(k => {
    if (!o[k]) {
      throw new Error(`Neither \`opts.${k}\` nor ${defaults[k]} environment`
                    + ` variable are configured!`);
    }
  });

  const call = bent(`https://${o.host}:${o.port}`, `POST`, `json`, 200, {
    'X-API-Key': o.apiKey,
  });

  const rpcReady = async () => {
    await waitPort({ host: o.host, port: parseInt(o.port, 10) });
  };

  const rpc = async (m, ...args) => {
    const lab = `RPC ${m} ${JSON.stringify(args)}`
    console.log(`${lab}`);
    const ans = await call(m, args);
    console.log(`${lab} ==> ${JSON.stringify(ans)}`);
    return ans;
  };

  const rpcCallbacks = async (m, arg, cbacks) => {
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

  return { rpc, rpcReady, rpcCallbacks };
};
