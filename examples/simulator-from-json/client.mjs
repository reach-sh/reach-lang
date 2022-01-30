const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');
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
