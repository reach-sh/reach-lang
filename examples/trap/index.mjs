import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.default.mjs';

const stdlib = loadStdlib(process.env);
const exports = backend.getExports(stdlib);
const x = stdlib.bigNumberify(2);
// ITE with impure arithmetic is now transformed to a localIf
stdlib.assert(x.eq(exports.trap(false, x)), "No longer traps");
