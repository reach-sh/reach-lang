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
  console.log("Testing Complete!")
}

main()
