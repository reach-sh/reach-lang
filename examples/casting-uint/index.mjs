import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const x = stdlib.bigNumberify(1).shl(256).sub(1);
const y = stdlib.bigNumberify(1).shl(255).or(1);
const x_truncated = stdlib.cast('UInt256', 'UInt', x, true);
const y_truncated = stdlib.cast('UInt256', 'UInt', y, true);

console.log(`x: ${x}`);
console.log(`y: ${y}`);
console.log(`x_truncated: ${x_truncated}`);
console.log(`y_truncated: ${y_truncated}`);
