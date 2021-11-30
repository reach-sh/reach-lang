export const mkAssertEq = rpc => async (l, e, a) => {
  const f = x => JSON.stringify(x).replace(/\\u0000/g, '');
  const [ ej, aj ] = [ f(e), f(a) ];
  if (ej === aj) {
    console.log(`${l} ${ej} === ${aj}`);
  } else {
    console.log(`${l} *** Mismatch! ***`);
    console.log(`  Expected: ${ej}`);
    console.log(`    Actual: ${aj}`);
  }
  await rpc('/stdlib/assert', ej === aj);
};
