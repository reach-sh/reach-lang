import waitPort from 'wait-port';
import bent     from 'bent';

export const mkRPC = async (opts = {}) => {

  const optOf = (field, envvar, modifiers) => {
    const mod = Object.assign({ def: undefined, f: x => x }, modifiers);
    const opt = opts[field]         !== undefined ? mod.f(opts[field])
              : process.env[envvar] !== undefined ? mod.f(process.env[envvar])
              : mod.def;

    if (opt === undefined) // `null` is acceptable here
      throw new Error(`Mandatory configuration unset for: ${field}`);

    return opt;
  };

  const host    = optOf('host',    'REACH_RPC_SERVER');
  const port    = optOf('port',    'REACH_RPC_PORT');
  const key     = optOf('key',     'REACH_RPC_KEY');
  const timeout = optOf('timeout', 'REACH_RPC_TIMEOUT',               { def: 5 });
  const verify  = optOf('verify',  'REACH_RPC_TLS_REJECT_UNVERIFIED', { f:   x => x !== '0' });

  if (!verify)
    process.env['NODE_TLS_REJECT_UNAUTHORIZED'] = '0';

  const call = bent(`https://${host}:${port}`, `POST`, `json`, 200, {
    'X-API-Key': key,
  });

  const debug = s =>
    process.env.REACH_DEBUG && console.log(s);


  const rpc = async (m, ...args) => {
    const lab = `RPC ${m} ${JSON.stringify(args)}`
    debug(`${lab}`);
    const ans = await call(m, args);
    debug(`${lab} ==> ${JSON.stringify(ans)}`);
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


  await waitPort({ host, port: parseInt(port, 10), timeout: timeout * 1000 });

  return { rpc, rpcCallbacks };
};
