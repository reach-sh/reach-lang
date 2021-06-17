import { loadStdlib } from '@reach-sh/stdlib';

(async () => {
  const stdlib = await loadStdlib();
  console.log(`Compile-only demo`);
})();
