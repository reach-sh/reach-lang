import express from 'express';
import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const makeHandle = (container) => (val) => {
    const id = container.length;
    container[id] = val;
    return id;
  };

  const ACC = [];
  const makeACC = makeHandle(ACC);
  const CTC = [];
  const makeCTC = makeHandle(CTC);
  const real_stdlib = await loadStdlib();
  // XXX bigNumberify in this object should be pushed into the standard library
  const rpc_stdlib = {
    ...real_stdlib,
    "newTestAccount": (async (bal) =>
      makeACC(await real_stdlib.newTestAccount(real_stdlib.bigNumberify(bal)))),
    "balanceOf": (async (id) =>
      await real_stdlib.balanceOf(ACC[id])),
    "formatCurrency": (async (x, y) =>
      await real_stdlib.formatCurrency(real_stdlib.bigNumberify(x), y)),
    "bigNumbertoNumber": (async (x) =>
      real_stdlib.bigNumberify(x).toNumber()),
  };
  const rpc_acc = {
    "attach": (async (id, ...args) =>
      makeCTC(await ACC[id].attach(backend, ...args))),
    "deploy": (async (id) =>
      makeCTC(await ACC[id].deploy(backend))),
  };
  const rpc_ctc = {
    "getInfo": (async (id) =>
      await CTC[id].getInfo()),
  };

  const app = express();

  const makeRPC = (olab, obj) => {
    const router = express.Router();
    for (const k in obj) {
      router.post(`/${k}`, async (req, res) => {
        const args = req.body;
        const lab = `RPC ${olab}/${k} ${JSON.stringify(args)}`;
        console.log(`${lab}`);
        const ans = await obj[k](...args);
        console.log(`${lab} ==> ${JSON.stringify(ans)}`);
        res.json(ans); });
    }
    return router;
  };

  const KONT = [];
  const makeKont = makeHandle(KONT);
  const route_backend = express.Router();
  for (const b in backend) {
    route_backend.post(`/${b}`, async (req, res) => {
      let lab = `RPC backend/${b}`;
      console.log(`${lab} IN`);
      const [ cid, vals, meths ] = req.body;
      const ctc = CTC[cid];
      const kid = makeKont(res);
      lab = `${lab} ${cid} ${kid}`;
      console.log(`${lab} START ${JSON.stringify(req.body)}`);
      let io = { ...vals };
      if ( io["stdlib.hasRandom"] ) {
        delete io["stdlib.hasRandom"];
        io = { ...real_stdlib.hasRandom, ...io };
      }
      for ( const m in meths ) {
        io[m] = (...args) => new Promise((resolve, reject) => {
          console.log(`${lab} IO ${m} ${JSON.stringify(args)}`);
          const old_res = KONT[kid];
          KONT[kid] = {resolve, reject};
          old_res.json({t: `Kont`, kid, m, args});
        });
      }
      const ans = await backend[b](ctc, io);
      console.log(`${lab} END ${JSON.stringify(ans)}`);
      const new_res = KONT[kid];
      KONT[kid] = null;
      console.log(`${lab} DONE`);
      new_res.json({t: `Done`, ans});
    });
  }
  const do_kont = (req, res) => {
    let lab = `KONT`;
    console.log(`${lab} IN`);
    const [ kid, ans ] = req.body;
    lab = `${lab} ${kid}`;
    console.log(`${lab} ANS ${JSON.stringify(ans)}`);
    const { resolve, reject } = KONT[kid];
    void (reject);
    KONT[kid] = res;
    console.log(`${lab} OUT`);
    resolve(ans);
  };

  // app.use((req, res, next) => {
  //   console.log(`LOG ${req.url}`);
  //   next(); });
  app.use(express.json());
  // app.use((req, res, next) => {
  //   console.log(`LOGb ${req.url} ${req.body}`);
  //   next(); });
  app.use(`/stdlib`, makeRPC('stdlib', rpc_stdlib));
  app.use(`/acc`, makeRPC('acc', rpc_acc));
  app.use(`/ctc`, makeRPC('ctc', rpc_ctc));
  app.use(`/backend`, route_backend);
  app.post(`/kont`, do_kont);
  app.post(`/stop`, (req, res) => {
    res.json(true);
    process.exit(0); });

  app.listen(process.env.REACH_RPC_PORT, () => {
    console.log(`I am alive`);
  });

})();
