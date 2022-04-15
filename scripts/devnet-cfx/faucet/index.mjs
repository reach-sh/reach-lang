import Timeout from 'await-timeout';
import cfxsdk from 'js-conflux-sdk';
import { createServer } from 'http';
import express from 'express';
import cors from 'cors';
import * as fs from 'fs';
import toml from 'toml';
import { Channel } from 'async-csp';

const debug = (...args) => console.log('FAUCET', new Date(), ...args);
const wait = () => Timeout.set(500);

const PORT = 1337;

const config_s = fs.readFileSync(process.env.CFX_CONFIG);
const config_o = toml.parse(config_s);
debug(config_o);
const { chain_id, mining_key } = config_o;

const conflux = new cfxsdk.Conflux({
  url: `http://localhost:12537`,
  networkId: chain_id,
});
const faucet = conflux.wallet.addPrivateKey(mining_key);
debug({ conflux, faucet });
const fund = new Channel();
fund.consume(async ({to, value, res}) => {
  let r;
  try {
    while ( !r ) {
      const txn = { from: faucet.address, to, value };
      debug({txn});
      let th;
      while ( ! th ) {
        try {
          th = await conflux.sendTransaction(txn);
        } catch (e) {
          if (  e.code === 'ECONNREFUSED'
             || e.code === -32077
             ) {
            debug('retry', e);
            await wait();
            continue;
          }
          throw e;
        }
      }
      debug({th});
      while ( true ) {
        r = await conflux.getTransactionReceipt(th);
        if (  ! r ) {
          debug('wait');
          await wait();
          continue;
        }
        break;
      }
      if ( r.outcomeStatus !== 0 ) {
        debug('failed, try again');
        await wait();
        r = undefined;
      }
    }
    debug({r});
    res.send(r);
  } catch (e) {
    debug({e});
    res.status(500).send(e);
  }
});

const app = express();
app.use(cors());
app.use(express.json());
app.get('*', (req, res, next) => {
  const { address: to, amount: value } = req.query;
  fund.put({to, value, res});
});

const opts = {
  allowHTTP1: true,
};
const server = createServer(opts, app);
server.on('error', (err) => debug('ERROR', err));

server.listen(PORT, () => {
  debug('Alive');
});
