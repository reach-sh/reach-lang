import { mkRPC }      from '@reach-sh/rpc-client';
import { mkAssertEq } from './common.mjs';

const { rpc, rpcCallbacks } = await mkRPC();
const assertEq              = mkAssertEq(rpc);
const sbal                  = await rpc('/stdlib/parseCurrency', 100);
const accAlice              = await rpc('/stdlib/newTestAccount', sbal);
const ctcAlice              = await rpc('/acc/contract', accAlice);
const decimals              = 5;

await rpcCallbacks('/backend/Alice', ctcAlice, {
  decimals,
  checkDecimals: async (tokId) => {
    const m = await rpc('/acc/tokenMetadata', accAlice, tokId);
    const d = a => rpc('/stdlib/bigNumberToNumber', a);
    assertEq('Metadata decimals should match', await d(m.decimals), await d(decimals));
  }
});

await Promise.all([
  rpc('/forget/ctc', ctcAlice),
  rpc('/forget/acc', accAlice),
]);
