const c = await import('@reach-sh/simulator-client');
const assert = await import('assert');
import { readFile } from 'fs/promises';

const script = JSON.parse(
  await readFile(
    new URL('./client.json', import.meta.url)
  )
);

const main = async () => {
  const port = 3001
  console.log("Init Testing!")
  const params = {
    port: port
  };
  await c.waitForPort(params)
  await c.interp(script)
  const r = await c.getStatus()
  assert.equal(r,"Done");
  console.log("Testing Complete!")
}

main()
