import { createSecureServer       } from 'http2';
import { randomBytes              } from 'crypto';
import { readFileSync, existsSync } from 'fs';
import { resolve                  } from 'path';

import express, { Request, Response, NextFunction } from 'express';

import { loadStdlib } from './loader';
import { debug      } from './shared_impl';

const withApiKey = () => {
  const key = process.env.REACH_RPC_KEY;

  if (!key) {
    console.error(
      [ '\nPlease populate the `REACH_RPC_KEY` environment variable with a'
      , ' strong pre-shared key, e.g.:\n'
      , '  $ head -c 24 /dev/urandom | base64\n'
      ].join(''));
    process.exit(1);
  }

  return (req: Request, res: Response, next: NextFunction) =>
    req.get('X-API-Key') === key
      ? next()
      : res.status(403).json({});
};


export const mkKont = () => {
  // TODO consider replacing stringly-typed exceptions with structured
  // descendants of `Error` base class
  const UNTRACKED = 'Untracked continuation ID:';
  const untracked = (i: string) => `${UNTRACKED} ${i}`;
  const k: any    = {};
  let   i: number = 0;

  const mkWas = (m: string) => (e: Error): boolean =>
    !!(e.message
      .substr(0, m.length)
      .match(`^${m}$`));

  const was = {
    untracked: mkWas(UNTRACKED),
  };

  const raise = (e: string) => {
    throw new Error(e);
  };

  const track = async (a: any) => {
    const rb = await randomBytes(24);
    const id = `${i}_${rb.toString('hex')}`;
    k[id]    = a;
    i++;

    return id;
  };

  const id = (i: string) =>
    k[i] === undefined
      ? raise(untracked(i))
      : k[i];

  const replace = (i: string, a: any) =>
    k[i] === undefined
      ? raise(untracked(i))
      : (() => { k[i] = a; return i; })();

  const forget = (i: string) =>
    delete k[i];

  return {
    // Internals
    _: {
      k,
      i,
      UNTRACKED,
      untracked,
    },

    // General API
    forget,
    id,
    replace,
    track,
    was,
  };
};


export const mkStdlibProxy = async (lib: any, ks: any) => {
  const { account, token } = ks;

  return {
    ...lib,

    newTestAccount: async (bal: any) =>
      account.track(await lib.newTestAccount(bal)),

    newTestAccounts: async (num: number, bal: any) =>
      Promise.all((await lib.newTestAccounts(num, bal)).map(account.track)),

    getDefaultAccount: async () =>
      account.track(await lib.getDefaultAccount()),

    newAccountFromSecret: async (s: string) =>
      account.track(await lib.newAccountFromSecret(s)),

    newAccountFromMnemonic: async (s: string) =>
      account.track(await lib.newAccountFromMnemonic(s)),

    createAccount: async () =>
      account.track(await lib.createAccount()),

    fundFromFaucet: (id: string, bal: any) =>
      lib.fundFromFaucet(account.id(id), bal),

    connectAccount: async (id: string) =>
      account.track(await lib.connectAccount(account.id(id).networkAccount)),

    balanceOf: async (id: string, token?: any) => {
      const t = token === undefined ? undefined
              : token.id            ? token.id // From `launchToken`
              : token;
      return await lib.balanceOf(account.id(id), t);
    },

    transfer: async (from: string, to: string, bal: any) =>
      lib.transfer(account.id(from), account.id(to), bal),

    assert: (x: any) =>
      lib.assert(x),

    // As of 2021-12-08 `launchToken` isn't officially documented
    // These are unlike `Token` values but we'll track them together, with the
    // intention that functions like `tokenAccept` should accept either
    launchToken: async (id: string, name: string, sym: string, opts: any = {}) => {
      const t = await lib.launchToken(account.id(id), name, sym, opts);
      return { kid: await token.track(t), token: t };
    },

    setQueryLowerBound: (nt: any) =>
      lib.setQueryLowerBound(lib.bigNumberify(nt)),
  };
};


export const serveRpc = async (backend: any) => {
  const account       = mkKont();
  const contract      = mkKont();
  const token         = mkKont();
  const kont          = mkKont();
  const real_stdlib   = await loadStdlib();
  const rpc_stdlib    = await mkStdlibProxy(real_stdlib, { account, token });
  const app           = express();
  const route_backend = express.Router();

  // `isBigNumber` in stdlib uses type reflection which doesn't work here due
  // to `n` having been serialized into JSON
  const reBigNumberify = (n: any) =>
    n && n.hex && n.type && n.type === 'BigNumber'
      ? (() => { try { return real_stdlib.bigNumberify(n); } catch (e) { return n; }})()
      : n;

  const rpc_acc = {
    contract: async (id: string, ...args: any[]) =>
      contract.track(await account.id(id).contract(backend, ...args)),

    attach: async (id: string, ...args: any[]) =>
      contract.track(await account.id(id).attach(backend, ...args)),

    deploy: async (id: string) =>
      contract.track(await account.id(id).deploy(backend)),

    getAddress: async (id: string) =>
      await account.id(id).getAddress(),

    setGasLimit: async (id: string, ...args: any[]) =>
      await account.id(id).setGasLimit(...args),

    setDebugLabel: async (id: string, l: string) =>
      account.id(id).setDebugLabel(l),

    tokenAccept: async (acc: string, tok: string) => {
      const t = token.id(tok);
      await account.id(acc).tokenAccept(t.id ? t.id : t);
      return null;
    },

    tokenAccepted: async (acc: string, tok: string) => {
      const t = token.id(tok);
      return await account.id(acc).tokenAccepted(t.id ? t.id : t);
    },
  };

  const rpc_ctc = {
    getInfo: async (id: string) =>
      contract.id(id).getInfo(),
  };

  const rpc_launchToken = {
    mint: async (kid: string, accTo: string, amt: any) =>
      token.id(kid).mint(account.id(accTo), real_stdlib.bigNumberify(amt)),

    optOut: async (kid: string, accFrom: string, accTo?: string) =>
      token.id(kid).optOut(account.id(accFrom), accTo ? account.id(accTo) : undefined),
  };

  const safely = (f: any) => (req: Request, res: Response) => (async (): Promise<any> => {
    const { was } = kont;

    const client =
      `client ${req.ip}: ${req.method} ${req.originalUrl} ${JSON.stringify(req.body)}`;

    try {
      debug(`Attempting to process request by ${client}`);
      await f(req, res);

    } catch (e: any) {
      debug(`!! Witnessed exception triggered by ${client}:\n  ${e.stack}`);

      const [ s, message ]
        = was.untracked(e) ? [ 404, String(e) ]
        :                    [ 500, 'Unspecified fault' ];

      if (!res.headersSent) {
        res.status(s).json({ message, request: req.body });
        debug(`!! HTTP ${s}: "${message}" response sent to client`);
      } else {
        res.end();
        debug(`!! Response already initiated; unable to send appropriate payload`);
      }
    }
  })();

  const mkRPC = (olab: string, obj: any) => {
    const router = express.Router();

    for (const k in obj) {
      router.post(`/${k}`, safely(async (req: Request, res: Response) => {
        const args = req.body;
        const lab  = `RPC /${olab}/${k} ${JSON.stringify(args)}`;
        debug(lab);

        const ans = await obj[k](...args);
        const ret = ans === undefined ? null : ans;
        debug(`${lab} ==> ${JSON.stringify(ret)}`);

        res.json(ret);
      }));
    }
    return router;
  };

  // `hasOwnProperty` is important for denying access to prototype fields
  const userDefinedField = (a: any, m: string) =>
    a && a.hasOwnProperty && a.hasOwnProperty(m) && a[m] || null;

  const mkUserDefined = (olab: string, prop: string, k: any, unsafe: boolean) => {
    const router = express.Router();
    router.post(/^\/(.*)/, safely(async (req: Request, res: Response) => {
      if (!Array.isArray(req.body))
        throw new Error(`Expected an array but received: ${req.body}`);

      const [ id, ...args ] = req.body;
      const lab = `RPC ${olab}${req.path} ${JSON.stringify(req.body)}`;
      debug(lab);

      try {
        const a = await req.path.split('/')
          .filter(a => a !== '')
          .reduce(userDefinedField, k.id(id)[prop])(...args.map(reBigNumberify));

        debug(`${lab} ==> ${JSON.stringify(a)}`);
        return res.json(a);
      } catch (e: any) {
        if (unsafe) {
          debug(`${lab} ==> ${JSON.stringify(e)}`);
          return res.status(404).json({});
        } else {
          debug(`${lab} ==> ${JSON.stringify(null)}`);
          return res.json(null);
        }
      }
    }));
    return router;
  };

  route_backend.post(/^\/getExports\/(.*)/, safely(async (req: Request, res: Response) => {
    const args = req.body;
    if (!Array.isArray(args))
      throw new Error(`Expected an array but received: ${args}`);

    const lab = `RPC /backend${req.path}${args.length > 0 ? ' ' + JSON.stringify(args) : ''}`;
    debug(lab);

    const b = await req.path.split('/')
      .filter(a => a !== '')
      .slice(1) // drop `getExports` path root
      .reduce(userDefinedField, await backend.getExports(real_stdlib));

    const a = typeof b === 'function'
      ? await b(...args)
      : b;

    debug(`${lab} ==> ${JSON.stringify(a)}`);
    res.json(a);
  }));

  const ctcPs: any = {};
  for (const b in backend) {
    const h = (lab: string) => safely(async (req: Request, res: Response) => {
      debug(`${lab} IN`);

      const [ cid, vals, meths ] = req.body;
      const ctc                  = contract.id(cid);
      const kid                  = await kont.track(res);
      lab                        = `${lab} ${cid} ${kid}`;

      debug(`${lab} START ${JSON.stringify(req.body)}`);
      let io = { ...vals };

      if (io['stdlib.hasRandom']) {
        delete io['stdlib.hasRandom'];
        io = { ...real_stdlib.hasRandom, ...io };
      }

      for (const m in meths) {
        io[m] = (...args: any[]) => new Promise((resolve, reject) => {
          debug(`${lab} IO ${m} ${JSON.stringify(args)}`);
          const old_res = kont.id(kid);

          kont.replace(kid, { resolve, reject });
          old_res.json({t: `Kont`, kid, m, args});
        });
      }

      const ans = await backend[b](ctc, io);
      debug(`${lab} END ${JSON.stringify(ans)}`);

      const new_res = kont.id(kid);
      kont.forget(kid);
      debug(`${lab} DONE`);

      new_res.json({t: `Done`, ans});
    });

    route_backend.post(`/${b}`, h(`RPC /backend/${b}`));
    ctcPs[`/ctc/p/${b}`           ] = h(`RPC /ctc/p/${b}`);
    ctcPs[`/ctc/participants/${b}`] = h(`RPC /ctc/participants/${b}`);
  }

  const do_kont = safely(async (req: Request, res: Response) => {
    let lab = `KONT`;
    debug(`${lab} IN`);

    const [ kid, ans ] = req.body;
    lab                = `${lab} ${kid}`;
    debug(`${lab} ANS ${JSON.stringify(ans)}`);

    const { resolve, reject } = kont.id(kid);
    void (reject);
    kont.replace(kid, res);
    debug(`${lab} OUT`);

    resolve(ans);
  });

  const mkForget = (K: any) => safely(async (req: Request, res: Response) => {
    req.body.map(K.forget);
    res.status(200).json({ deleted: req.body });
  });

  app.use(withApiKey());
  app.use(express.json());

  app.use(`/stdlib`,      mkRPC('stdlib',      rpc_stdlib));
  app.use(`/acc`,         mkRPC('acc',         rpc_acc));
  app.use(`/ctc`,         mkRPC('ctc',         rpc_ctc));
  app.use(`/launchToken`, mkRPC('launchToken', rpc_launchToken));
  app.use(`/backend`,     route_backend);

  // NOTE: since `getViews()` is deprecated we deliberately skip it here
  app.use(`/ctc/v`,           mkUserDefined('/ctc/v',           'v',           contract, false));
  app.use(`/ctc/views`,       mkUserDefined('/ctc/views',       'views',       contract, false));
  app.use(`/ctc/unsafeViews`, mkUserDefined('/ctc/unsafeViews', 'unsafeViews', contract, true));

  app.use(`/ctc/a`,           mkUserDefined('/ctc/a',           'a',           contract, true));
  app.use(`/ctc/apis`,        mkUserDefined('/ctc/apis',        'apis',        contract, true));
  app.use(`/ctc/safeApis`,    mkUserDefined('/ctc/safeApis',    'safeApis',    contract, false));

  // NOTE: it's important these are deferred in order to preserve middleware precedence
  for (const p in ctcPs) { app.use(p, ctcPs[p]); }

  app.post(`/kont`, do_kont);

  // NOTE: successful `/backend/<participant>` requests automatically `forget`
  // their continuation ID before yielding a "Done" response; likewise with
  // requests to `/kont` due to their relationship with `/backend/<participant>`
  app.post(`/forget/acc`,   mkForget(account));
  app.post(`/forget/ctc`,   mkForget(contract));
  app.post(`/forget/token`, mkForget(token));

  app.post(`/stop`, safely(async (_: Request, res: Response) => {
    res.json(true);
    process.exit(0);
  }));

  app.post(`/health`, safely(async (req: Request, res: Response) => {
    void(req);
    res.json(true);
  }));

  app.disable('x-powered-by');

  const fetchOrFail = (envvar: string, desc: string) => {
    const f = process.env[envvar];

    if (!f) {
      console.error(
        [ `\nPlease populate the \`${envvar}\` environment variable with`
        , ` the path to your TLS ${desc}.\n`
        ].join(''));
      process.exit(1);
    }

    const fq = resolve(`./tls/${f}`);

    if (!existsSync(fq)) {
      console.error(`\nPath: ${fq} does not exist!\n`);
      process.exit(1);
    }

    return readFileSync(fq);
  };

  const opts = {
    allowHTTP1: true,
    key:        fetchOrFail('REACH_RPC_TLS_KEY', 'private key'),
    cert:       fetchOrFail('REACH_RPC_TLS_CRT', 'public certificate'),
  };

  const passphrase = process.env.REACH_RPC_TLS_PASSPHRASE;
  if (passphrase)
    Object.assign(opts, { passphrase });

  // @ts-ignore
  createSecureServer(opts, app)
    .listen(process.env.REACH_RPC_PORT, () =>
      debug(`I am alive`));
};
