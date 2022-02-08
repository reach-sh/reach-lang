import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = loadStdlib();
  console.log(`Compile-only demo`);
})();
