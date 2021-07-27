import { getIndexer } from "./ALGO";
import { debug } from "./shared_impl";
import Timeout from 'await-timeout';

export type ApiCall<T> = {
  do: () => Promise<T>,
};

export type QueryResult =
  | { succ: true, txn: any }
  | { succ: false, round: number }

export function looksLikeAccountingNotInitialized(e: any) {
  const responseText = e?.response?.text || null;
  // TODO: trust the response to be json and parse it?
  // const json = JSON.parse(responseText) || {};
  // const msg: string = (json.message || '').toLowerCase();
  const msg = (responseText || '').toLowerCase();
  return msg.includes(`accounting not initialized`);
}

export const doQuery_ = async <T>(dhead:string, query: ApiCall<T>, alwaysRetry: boolean = false): Promise<T> => {
  debug(dhead, '--- QUERY =', query);
  let retries = 10;
  let res;
  while ( retries > 0 ) {
    try {
      res = await query.do();
      break;
    } catch (e) {
      if ( e?.errno === -111 || e?.code === "ECONNRESET") {
        debug(dhead, 'NO CONNECTION');
      } else if ( looksLikeAccountingNotInitialized(e) ) {
        debug(dhead, 'ACCOUNTING NOT INITIALIZED');
      } else if ( ! alwaysRetry || retries <= 0 ) {
        throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`);
      }
      debug(dhead, 'RETRYING', retries--, {e});
      await Timeout.set(500);
    }
  }
  if (!res) { throw Error(`impossible: query res is empty`); }
  debug(dhead, '--- RESULT =', res);
  return res;
};

export const chooseMinRoundTxn = (ptxns: any[]) =>
  ptxns.reduce((accum: any, x: any) =>
      (x['confirmed-round'] < accum['confirmed-round']) ? x : accum, ptxns[0])

export type RoundInfo = {
  minRound?: number,
  maxRound?: number,
  specRound?: number,
}

export class EventCache {

  cache: any[] = [];

  currentRound = 0;

  constructor() {
    this.cache = [];
  }

  async query(dhead: string, ApplicationID: number, roundInfo: RoundInfo, pred: ((x:any) => boolean)): Promise<QueryResult> {
    const { minRound, maxRound, specRound } = roundInfo;
    debug(dhead, `EventCache.query`, ApplicationID, minRound, specRound, maxRound, this.currentRound);

    // Clear cache of stale transactions
    const filterRound = minRound || specRound || 0;
    this.cache = this.cache.filter(x => x['confirmed-round'] > filterRound);

    // Check to see if the transaction we want is in cache
    const initPtxns = this.cache.filter(pred);
    if (initPtxns.length != 0) {
      debug(`Found transaction in Event Cache`);
      const txn = chooseMinRoundTxn(initPtxns)
      this.currentRound = txn['confirmed-round'];
      return { succ: true, txn };
    }

    debug(`Transaction not in Event Cache. Querying network...`);

    // If no results, then contact network
    const indexer = await getIndexer();

    let query =
      indexer.searchForTransactions()
        .applicationID(ApplicationID)
        .txType('appl')

    if (minRound) {
      query = query.minRound(minRound + 1);
    }
    if (maxRound) {
      query = query.maxRound(maxRound);
    }
    if (specRound) {
      query = query.round(specRound);
    }

    const res: any = await doQuery_(dhead, query);
    this.cache = res.transactions;
    this.currentRound = res['current-round'];

    // Check for pred again
    const ptxns = this.cache.filter(pred);

    if ( ptxns.length == 0 ) {
      return { succ: false, round: this.currentRound };
    }

    const txn = chooseMinRoundTxn(ptxns);

    return { succ: true, txn };
  }
}
