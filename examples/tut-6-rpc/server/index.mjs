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
  const rpc_stdlib = {
    ...real_stdlib,
    "newTestAccount": (async (bal) =>
      makeACC(await real_stdlib.newTestAccount(real_stdlib.bigNumberify(bal)))),
    "balanceOf": (async (id) =>
      await real_stdlib.balanceOf(ACC[id])),
    "formatCurrency": (async (x, y) =>
      await real_stdlib.formatCurrency(real_stdlib.bigNumberify(x), y)),
  };
  const rpc_acc = {
    "attach": (async (id, ...args) =>
      makeCTC(await ACC[id].attach(...args))),
    "deploy": (async (id) =>
      makeCTC(await ACC[id].deploy())),
  };
  const rpc_ctc = {
    "getInfo": (async (id) =>
      await CTC[id].getInfo()),
  };

  console.log(`I am the server`);

  const app = express();

  const makeRPC = (obj) => {
    const router = express.Router();
    for (const k in obj) {
      router.post(`/${k}`, async (req, res) => {
        const args = req.body;
        const lab = `RPC ${k} ${JSON.stringify(args)}`;
        console.log(`${lab}`);
        const ans = await obj[k](...args);
        console.log(`${lab} ==> ${JSON.stringify(ans)}`);
        res.json(ans); });
    }
    return router;
  };

  const route_backend = express.Router();
  for (const b in backend) {
    route_backend.post(`/${b}`, async (req, res) => {
      console.log(`XXX backend ${b}`);
      res.json(false);
    });
  }

  app.use((req, res, next) => {
    console.log(`LOG ${req.url}`);
    next(); });
  app.use(express.json());
  app.use((req, res, next) => {
    console.log(`LOGb ${req.url} ${req.body}`);
    next(); });
  app.use(`/stdlib`, makeRPC(rpc_stdlib));
  app.use(`/acc`, makeRPC(rpc_acc));
  app.use(`/ctc`, makeRPC(rpc_ctc));
  app.use(`/backend`, route_backend);
  app.post(`/quit`, (req, res) => {
    res.json(true);
    process.exit(0); });

  app.listen(process.env.REACH_RPC_PORT, () => {
    console.log(`I am alive`);
  });

})();
