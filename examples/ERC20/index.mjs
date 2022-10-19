
import { genericTests } from "./generic-tests.mjs";
import { ozComparisonTests } from "./oz-compare.mjs";

await genericTests();
await ozComparisonTests();


process.exit(0);

