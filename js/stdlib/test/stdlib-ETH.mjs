import { runTests, describe, it, expect } from '../tester.mjs';

import * as ETH    from '../ETH.mjs';
import * as ALGO   from '../ALGO.mjs';
import * as FAKE   from '../FAKE.mjs';
import * as common from './common.mjs';


runTests(async () => describe('The `ETH` stdlib', async () => {
  const { bigNumberify, formatCurrency, parseCurrency } = ETH;

  const sec = '9573fa33a57fee662a23bf60f1b1674364d99fb8dd2166b4ae470ce7ab20ed9f';

  await common.mkStdlibNetworkCommon(ETH);
  await common.mkGetDefaultAccount(ETH);
  await common.mkNewAccountFromSecret(ETH, 14, sec);
  // TODO await common.mkNewAccountFromMnemonic(ETH, 14, mon);
  await common.mkConnectAccount(ETH, a => a.networkAccount);
  await common.mkFundFromFaucet(ETH);


  await describe('T_Address.canonicalize can handle multiple inputs', async () => {
    const a = await ETH.createAccount();
    const e = ETH.T_Address.canonicalize(a);
    const f = (accessor, fields) =>
      common.mkT_AddressCanonicalize(ETH, a, accessor, fields, e);

    f(a => a,                        '');
    f(a => a.networkAccount,         '.networkAccount');
    f(a => a.networkAccount.address, '.networkAccount.address');
  });


  describe('exports', () => {
    const stdlibExports = Object.keys(ETH).sort();

    const ETH_extra_exports = [
      'setProvider',
    ];

    const ALGO_extra_exports = [
      'newAccountFromAlgoSigner',
      'setAlgodClient',
      'setBrowser',
      'setIndexer',
      'setWaitPort',
    ];

    const FAKE_extra_exports = [];

    for (const [otherName, otherStdlib, otherExtraExports] of [
        ['ALGO', ALGO, ALGO_extra_exports],
        ['FAKE', FAKE, FAKE_extra_exports],
      ]) {

      const otherStdlibExports = Object.keys(otherStdlib).sort();

      it(`should only export a few extra things compared to ${otherName}`, () =>
        expect(stdlibExports.filter(x => !otherStdlibExports.includes(x)))
          .toBe(ETH_extra_exports));

      it(`should export everything that ${otherName} does`, () =>
        expect(otherStdlibExports.filter(x => !stdlibExports.includes(x)))
          .toBe(otherExtraExports));
    }
  });


  describe('currency conversions', () => {
    const amt = '123.456';
    // 3 is the number of decimals to shift over to make 123.456 a whole number
    const amtWei      = bigNumberify('10').pow(18 - 3).mul('123456');
    const amtTruncTo2 = amt.slice(0, amt.length - 1); // truncates! does not round

    it(`should convert to WEI correctly`, () =>
      expect(parseCurrency(amt)).toBe(amtWei));

    it(`should convert from WEI correctly`, () => {
      expect(formatCurrency(amtWei)).toBe(amt);
      expect(formatCurrency(amtWei, 2)).toBe(amtTruncTo2);
      expect(formatCurrency(amtWei.toString())).toBe(amt);
      expect(formatCurrency(789)).toBe('0.000000000000000789');
    });
  });
}));
