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
    ...real_stdlib
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
      router.post(`/${k}`, express.json, async (req, res) => {
        const lab = `RPC ${k} ${JSON.stringify(args)}`;
        console.log(`${lab}`);
        const ans = await obj[k](...req.body);
        console.log(`${lab} ==> ${JSON.stringify(ans)}`);
        res.json(ans); });
    }
    return router;
  };

  const route_backend = express.Router();
  for (const b in backend) {
    route_backend.post(`/${b}`, express.json, async (req, res) => {
      console.log(`XXX backend ${b}`);
      res.json(false);
    });
  }

  app.use((req, res, next) => {
    console.log(`LOG ${req.url}`);
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
