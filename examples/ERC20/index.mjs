import { loadStdlib } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);
if (stdlib.connector !== "ETH") {
  console.log("This example is only tested against ETH for now.");
  process.exit(0);
}

import { genericTests } from "./generic-tests.mjs";
await genericTests();

import { ozComparisonTests } from "./oz-compare.mjs";
await ozComparisonTests();

process.exit(0);
