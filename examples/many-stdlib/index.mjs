import { loadStdlib } from '@reach-sh/stdlib';

const stdlib1 = loadStdlib();
const stdlib2 = loadStdlib();

stdlib1.assert(stdlib1 !== stdlib2, "stdlibs are different");
