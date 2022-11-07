import * as c from '@reach-sh/simulator-client';
import * as  assert from 'assert';
import { readFile } from 'fs/promises';

const script = JSON.parse(
  await readFile(
    new URL('./client.json', import.meta.url)
  )
);

const main = async () => {
  console.log("Init Testing!")
  await c.waitForPort()
  await c.interp(script)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  const l = await c.getStateGlobals(13)
  const amt = l.e_ledger["0"]["-1"]
  // Alice
  assert.equal(amt,22);
  console.log("Testing Complete!")
}

main()
