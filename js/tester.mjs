import { strict as assert } from 'assert';
export { strict as assert } from 'assert';

const AsyncFunction = (async () => {}).constructor;

export const runTests = (f) => {
  if (f instanceof AsyncFunction) {
    f().then(() => process.exit(0)).catch((e) => { console.log(e); process.exit(1); }); }
  else {
    f(); } };

export const describe = (label, f) => {
  console.log(label);
  return f(); };

export const it = describe;

export const expect = (x) => ({
  toBe: (v => assert.deepEqual(x, v)) });
