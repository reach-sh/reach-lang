import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const [accAlice, accBob] = await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));
const cap = await stdlib.launchToken(accAlice, "Cap", "cap");
const bit = await stdlib.launchToken(accBob, "Bit", "bit");

const check = async (acc, tok) => {
  const accName = acc == accAlice ? 'Alice' : 'Bob';
  const toks = await (acc == accAlice ? acc.tokensAccepted() : stdlib.tokensAccepted(acc));
  const isAccepted = toks.some(t => tok.id.eq(t));
  stdlib.assert(isAccepted, `${tok.name} accepted by ${accName}`);
};

if (stdlib.connector === "ALGO") {
  await check(accAlice, cap);
  await check(accBob, bit);
  await accAlice.tokenAccept(bit.id);
  await accBob.tokenAccept(cap.id);
  await check(accAlice, cap);
  await check(accAlice, bit);
  await check(accBob, cap);
  await check(accBob, bit);
} else {
  // ETH and CFX do not keep track of accepted tokens
}
