import { genericTests } from "./generic-tests.mjs";
import { ozComparisonTests } from "./oz-compare.mjs";

if (stdlib.connector !== "ETH") {
  console.log("This example is only compiled against ETH for now".);
  process.exit(0);
}

await genericTests();
await ozComparisonTests();


process.exit(0);

