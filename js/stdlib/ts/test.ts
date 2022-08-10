import { process } from './shim';
import * as util from 'util';

// Use argument to just run one
const args = [ ...process.argv ];
while ( args.length > 0 && args[0] !== '---' ) {
  args.shift();
}
args.shift();
const shouldRunAny = (x:string): boolean =>
  args.some((a:string) => x.includes(a));
export const shouldRun = (x:string): boolean =>
  ((args.length === 0) || shouldRunAny(x));
export const shouldRunExac = (x:string): boolean =>
  ((args.length !== 0) && shouldRunAny(x));

// Basic tests
export type Job<T> = () => Promise<T>;
export type Xtra = {[key:string]: any};
interface Case extends Xtra {
  id: string,
  err?: string,
};
const cases: Array<Case> = [];
let tests = 0; let fails = 0;
const loud = true; //(args.length !== 0);
export const chk = (id:string, actual:any, expected:any, xtra:Xtra = {}) => {
  tests++;
  const xtras = JSON.stringify(xtra, null, 2);
  const exps = JSON.stringify(expected, null, 2);
  let acts = JSON.stringify(actual, null, 2);
  if ( acts === "{}" ) { acts = actual.toString(); }
  let show; let err;
  if ( exps !== acts ) {
    fails++;
    err = `${xtras}\nexpected ${exps}, got ${acts}`;
    show = 'FAIL';
  } else if ( loud ) {
    show = 'SUCC';
  }
  cases.push({id, time: xtra?.time, err});
  if ( show ) {
    if ( expected?._isBigNumber ) { expected = expected.toString(); }
    if ( actual?._isBigNumber ) { actual = actual.toString(); }
    console.log(show, util.inspect({ ...xtra, id, expected, actual }, {
      depth: null, sorted: true, colors: true,
    }));
  }
};
export const chkErr = async (id:string, exp:string, f:Job<void>, xtra:Xtra = {}) => {
  const clean = (s:string) => s.replace(/\0/g, '').replace(/\\u0000/g, '');
  const exps = clean(exp);
  try {
    const r = await f();
    throw Error(`Expected error, but got ${JSON.stringify(r)}`);
  } catch (e:unknown) {
    let es = (typeof e === 'object' && e !== null) ? e.toString() : `${e}`;
    if ( es === '[object Object]' ) {
      try { es = JSON.stringify(e); }
      catch (e) { void(e); }
    }
    es = clean(es);
    chk(id, es.includes(exps), true, { ...xtra, e, es, exps });
  }
};

const notErr = async <T>(lab: string, j:Job<T>, xtra: Xtra = {}): Promise<T|undefined> => {
  const start = Date.now();
  let r = undefined;
  let err = undefined;
  try {
    r = (await j());
  } catch (e) {
    err = e;
  }
  const end = Date.now();
  chk(lab, err, undefined, { ...xtra, time: ((end - start)/1000) });
  return r;
}

// Concurrent tests
const jobs: Array<Job<void>> = [];
export const one = (lab:string, j:Job<void>): void => {
  if ( shouldRun(lab) ) {
    jobs.push(async () => { await notErr(lab, j); });
  }
};

export interface RunOpts {
  howManyAtOnce?: number,
  exitOnFail?: boolean,
  noVarOutput?: boolean,
};
export const run = async (opts?:RunOpts): Promise<void> => {
  const exitOnFail = opts?.exitOnFail === undefined ? true : opts.exitOnFail;
  const stop = (): boolean => (exitOnFail && fails > 0);

  const howManyAtOnce = opts?.howManyAtOnce || 1;
  const noVarOutput = opts?.noVarOutput === undefined ? false : opts.noVarOutput;
  const varOutput = ! noVarOutput;

  console.log(`${jobs.length} jobs scheduled, running...`);
  while ( !stop() && jobs.length > 0 ) {
    console.log(`Spawning ${howManyAtOnce} of ${jobs.length} jobs`);
    const active = [];
    while ( jobs.length > 0 && active.length < howManyAtOnce ) {
      const j = jobs.shift();
      if ( j ) { active.push(j()); }
    }
    console.log(`Waiting for ${active.length} jobs`);
    await Promise.all(active);
  }
  console.log('Done running');

  const failed = fails !== 0;

  if ( varOutput ) {
    // Render XML output
    const xml = [];
    xml.push('<?xml version="1.0" encoding="UTF-8"?>');
    xml.push('<testsuite>');
    cases.forEach(({id, time, err}) => {
      const mtime = time ? ` time="${time}"` : ``;
      const mfail = err ? `<failure>${err}</failure>` : ``;
      const idr =
        id.replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/"/g, '&quot;')
          .replace(/'/g, '&apos;');
      xml.push(`<testcase name="${idr}"${mtime}>${mfail}</testcase>`);
    });
    xml.push('</testsuite>');
    const xmlb = Buffer.from(xml.join(''));

    // Output a summary
    const logVar = (k:string, v:string): void =>
      console.log(`var ${k}='${v}'`);

    logVar(`RESULTS_B64`, xmlb.toString('base64'));
    logVar(`SUMMARY`, failed ?
      `${fails} of ${tests} tests failed!` :
      `${tests} tests passed!`);
    logVar(`STATUS`, failed ? ':warning: FAIL' : ':pizza: OKAY');
  }
  process.exit( failed ? 1 : 0 );
};

// Exports
export const makeChkExport = (stdlib:any, backend:any): any => {
  const rshExports: {[fn:string]: any} = backend.getExports(stdlib);
  const chkExport = (fn:string, go:any) => {
    one(fn, async () => {
      const f = rshExports[fn];
      if (!f) {
        throw Error(`${fn} is not exported from backend: [${Object.keys(exports).sort().join(', ')}]`);
      }
      const mkId = (dom:any[]) => `${fn}(${JSON.stringify(dom)})`;
      const chkf = async (dom:any[], rng:any) => {
        const lab = mkId(dom);
        chk(lab, await notErr(lab, () => f(...dom), {dom}), rng, {dom});
      };
      const chkfErr = (exp:any, dom:any[]) =>
        chkErr(mkId(dom), exp, () => f(...dom), {dom});
      go(chkf, chkfErr)
    });
  };
  return [ rshExports, chkExport ];
};
