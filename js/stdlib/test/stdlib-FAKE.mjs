import { runTests, describe, it, expect } from '../tester.mjs';

import * as FAKE   from '../FAKE.mjs';
import * as common from './common.mjs';


runTests(async () => describe('The `FAKE` stdlib', async () => {
  const { bigNumberify, formatCurrency, parseCurrency } = FAKE;

  await common.mkStdlibNetworkCommon(FAKE);
  // TODO await common.mkGetDefaultAccount(FAKE);
  // TODO await common.mkNewAccountFromSecret(FAKE, 8, sec);
  // TODO await common.mkNewAccountFromMnemonic(FAKE, 8, mon);
  await common.mkConnectAccount(FAKE, a => a.networkAccount);
  await common.mkFundFromFaucet(FAKE);


  await describe('T_Address.canonicalize can handle multiple inputs', async () => {
    const a = await FAKE.createAccount();
    const e = FAKE.T_Address.canonicalize(a);
    const f = (accessor, fields) =>
      common.mkT_AddressCanonicalize(FAKE, a, accessor, fields, e);

    f(a => a,                        '');
    f(a => a.networkAccount,         '.networkAccount');
    f(a => a.networkAccount.address, '.networkAccount.address');
  });


  describe('currency conversions', () => {
    //// Excess is truncated; no such thing as fractional reachies.
    const amtReachiesStr = '123';
    const amtReachies    = bigNumberify(amtReachiesStr);

    it(`should convert to reachies correctly`, () =>
      expect(parseCurrency(amtReachiesStr)).toBe(amtReachies));

    it(`should convert from reachies correctly`, () => {
      expect(formatCurrency(amtReachies)).toBe(amtReachiesStr);
      // Again, no decimal places for reachies.
      expect(formatCurrency(amtReachies, 0)).toBe(amtReachiesStr);
      expect(formatCurrency(amtReachiesStr)).toBe(amtReachiesStr);
      expect(formatCurrency(789)).toBe('789');
    });
  });
}));
