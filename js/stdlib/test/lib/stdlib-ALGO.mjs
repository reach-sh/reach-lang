import * as ALGO   from '@reach-sh/stdlib/ALGO.mjs';
import * as common from './common.mjs';

const { describe, it, expect } = common;


export const spec = async () => describe('The `ALGO` stdlib', async () => {
  const { bigNumberify, formatCurrency, parseCurrency } = ALGO;

  await common.mkStdlibNetworkCommon(ALGO);
  // TODO await common.mkGetDefaultAccount(ALGO);
  // TODO await common.mkNewAccountFromSecret(ALGO,   8, sec);
  // TODO await common.mkNewAccountFromMnemonic(ALGO, 8, mon);
  await common.mkConnectAccount(ALGO, a => a.networkAccount);


  await describe('T_Address.canonicalize can handle multiple inputs', async () => {
    const a = await ALGO.createAccount();
    const e = ALGO.T_Address.canonicalize(a);
    const f = (accessor, fields) =>
      common.mkT_AddressCanonicalize(ALGO, a, accessor, fields, e);

    f(a => a,                '');
    f(a => a.networkAccount, '.networkAccount');
  });


  describe('currency conversions', () => {
    const amt         = '123.456';
    const amtTruncTo2 = amt.slice(0, amt.length - 1); // truncates! does not round

    // 3 is the number of decimals to shift over to make 123.456 a whole number
    const amtMicroAlgos = bigNumberify('10').pow(6 - 3).mul('123456');

    it(`should convert to microAlgos correctly`, () =>
      expect(parseCurrency(amt)).toBe(amtMicroAlgos));

    it(`should convert from microAlgos correctly`, () => {
      expect(formatCurrency(amtMicroAlgos)).toBe(amt);
      expect(formatCurrency(amtMicroAlgos, 2)).toBe(amtTruncTo2);
      expect(formatCurrency(amtMicroAlgos.toString())).toBe(amt);
      expect(formatCurrency(789)).toBe('0.000789');
    });
  });
});
