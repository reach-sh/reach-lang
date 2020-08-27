// Test runner.mjs
export async function main(stdlib, ...args) {
  console.log(Object.keys(stdlib));
  console.log(JSON.stringify(args));
}
