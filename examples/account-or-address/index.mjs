import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const hundred = stdlib.parseCurrency(100);
const [A, B] = await stdlib.newTestAccounts(2, hundred);
const tok = await stdlib.launchToken(A, "token", "tok");
await B.tokenAccept(tok.id);

const assertEq = (a, b) => stdlib.assert(
  a === b || a.eq?.(b) || a.every?.((v, i) => v === b[i] || v.eq?.(b[i])),
  `${a} == ${b}`
);

const testBoth = async (name, acc, f) => {
  console.log(`${name} Account`);
  const accRes = await f(acc);
  console.log(`${name} Address`);
  const addrRes = await f(acc.getAddress());
  return [accRes, addrRes];
}

const testBothEq = async (name, acc, f) => {
  const [accRes, addrRes] = await testBoth(name, acc, f);
  assertEq(accRes, addrRes);
};

await testBoth("token.mint", B, x => tok.mint(x, 1));
await testBoth("transfer #1", B, x => stdlib.transfer(A, x, 1, tok.id))
await testBoth("transfer #2", A, x => stdlib.transfer(B, x, stdlib.parseCurrency(1)));
await testBothEq("balanceOf A", A, x => stdlib.balanceOf(x, tok.id));
await testBothEq("balanceOf B", B, x => stdlib.balanceOf(x, tok.id));
await testBothEq("balancesOf A", A, x => stdlib.balancesOf(x, [null, tok.id]));
await testBothEq("balancesOf B", B, x => stdlib.balancesOf(x, [null, tok.id]));
await testBoth("fundFromFaucet A", A, x => stdlib.fundFromFaucet(x, hundred));
await testBoth("fundFromFaucet B", B, x => stdlib.fundFromFaucet(x, hundred));
await testBothEq("formatAddress A", A, x => stdlib.formatAddress(x));
await testBothEq("formatAddress B", B, x => stdlib.formatAddress(x));
await testBothEq("tokensAccepted A", A, x => stdlib.tokensAccepted(x));
await testBothEq("tokensAccepted B", B, x => stdlib.tokensAccepted(x));
