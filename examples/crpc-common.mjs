import chalk from 'chalk';

export const mkAssertEq = rpc => async (l, e, a) => {
  const f = x => JSON.stringify(x).replace(/\\u0000/g, '');
  const [ ej, aj ] = [ f(e), f(a) ];
  if (ej === aj) {
    console.log(chalk.green.bold(`\u2714 ${l}`));
  } else {
    console.log(chalk.red.bold(`\u2717 ${l} *** Mismatch! ***`));
  }
  console.log('  Expected:', JSON.parse(ej));
  console.log('    Actual:', JSON.parse(aj));
  await rpc('/stdlib/assert', ej === aj);
};
