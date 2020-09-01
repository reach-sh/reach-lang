import Timeout         from 'await-timeout';

import * as stdlib from './shared.mjs';
export * from './shared.mjs';

const DEBUG = false;
const debug = msg => { if (DEBUG) {
  console.log(`DEBUG@${BLOCK_NUMBER}: ${msg}`); } };

const REACHY_RICH = { address: 'reachy_rich' };

let BLOCK_NUMBER = 0;
// This can be exposed to the user for checking the trace of blocks
// for testing.
const BLOCKS = [];
const BALANCES = {};

export const balanceOf = async acc => BALANCES[acc.address];

export const transfer = async (to, from, value) => {
  const toa = to.address;
  const froma = from.address;
  stdlib.assert(stdlib.le(value, BALANCES[froma]));
  debug(`transfer ${froma} -> ${toa} of ${value}`);
  BLOCKS[BLOCK_NUMBER++] = { to: toa, from: froma, value };
  BALANCES[toa] = stdlib.add(BALANCES[toa], value);
  BALANCES[froma] = stdlib.sub(BALANCES[froma], value); };

export const connectAccount = async networkAccount => {
  const { address } = networkAccount;

  const attach = async (bin, ctc) => {
    let last_block = ctc.creation_block;

    const iam = (some_addr) => {
      if ( some_addr == address ) {
        return address;
      } else {
        throw Error(`I should be ${some_addr}, but am ${address}`);
      }
    };

    const wait = (delta) => {
      while ( BLOCK_NUMBER < (last_block + delta) ) {
        BLOCKS[ BLOCK_NUMBER++ ] = { type: 'wait' }; } };

    const sendrecv = async (label, funcNum, evt_cnt, args, value, timeout_delay, try_p) => {
      // XXX use try_p to figure out what transfers from the contract
      // to make, like in ALGO
      void(try_p);

      if ( ! timeout_delay || BLOCK_NUMBER < last_block + timeout_delay ) {
        debug(`${label} send ${funcNum} --- post`);
        transfer(ctc, networkAccount, value);
        BLOCKS[BLOCK_NUMBER-1].event = { funcNum, from: address, data: args.slice(-1 * evt_cnt), value, balance: BALANCES[ctc.address] };
        return await recv( label, funcNum, evt_cnt, timeout_delay ); }
      else {
        debug(`${label} send ${funcNum} --- timeout`);
        return { didTimeout: true }; } };

    const recv = async (label, funcNum, evt_cnt, timeout_delay) => {
      let check_block = last_block;
      while ( ! timeout_delay || check_block < last_block + timeout_delay ) {
        debug(`${label} recv ${funcNum} --- check ${check_block}`);
        const b = BLOCKS[check_block];
        if ( ! b || ! b.event || b.event.funcNum != funcNum ) {
          debug(`${label} recv ${funcNum} --- wait`);
          check_block = Math.min( check_block + 1, BLOCK_NUMBER );
          await Timeout.set(1);
          continue; }
        else {
          debug(`${label} recv ${funcNum} --- recv`);
          last_block = check_block;
          const evt = b.event;
          return { didTimeout: false, data: evt.data, value: evt.value, balance: evt.balance, from: evt.from }; } }

      debug(`${label} recv ${funcNum} --- timeout`);
      return { didTimeout: true }; };

    return { ...ctc, sendrecv, recv, iam, wait }; };

  const deploy = async (bin) => {
    const contract = makeAccount();
    debug(`new contract: ${contract.address}`);
    return await attach(bin, { ...contract,
                               creation_block: BLOCK_NUMBER,
                               events: {} });
  };

  return { deploy, attach, networkAccount }; };

const makeAccount =
      () => {
        const address = stdlib.random_uint256();
        BALANCES[address] = 0;
        return { address }; };

export const newTestAccount = async (startingBalance) => {
  const acc = makeAccount();
  debug(`new account: ${acc.address}`);
  BALANCES[REACHY_RICH.address] = startingBalance;
  transfer(acc, REACHY_RICH, startingBalance);
  return await connectAccount( acc ); };
