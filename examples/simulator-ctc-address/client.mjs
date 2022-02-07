const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');
const pj = await import('prettyjson')
import { readFile } from 'fs/promises';

const script = JSON.parse(
  await readFile(
    //
    // produced by the simulator ui 0.3.1
    new URL('./client.json', import.meta.url)
  )
);

const logGlobal = async (n) => {
  const r = await c.getStateGlobals(n)
  console.log(`LOGGING GLOBAL STATE ${n}`)
  console.log(pj.render(r))
  console.log("-------------------")
}

const logLocal = async (n) => {
  const r = await c.getStateLocals(n)
  console.log(`LOGGING LOCAL STATE ${n}`)
  console.log(pj.render(r))
  console.log("-------------------")
}

const getVar = async (v,n,aid) => {
  const x = await c.getStateLocals(n)
  return x.l_locals[aid].l_store.find(el => el[0].startsWith(v))
}

const assertVar = (y,t,v) => {
  assert.equal(y[1].tag,t);
  assert.equal(y[1].contents,v);
}

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.interp(script)
  let y = await getVar('address',15,1)
  let z = await getVar('ctcInfo',15,1)
  assertVar(y, 'V_Address', 1)
  assertVar(z,'V_Contract', 1)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
