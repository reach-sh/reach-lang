'reach 0.1';

export const main = Reach.App(
  {},
  [Participant('Alice', {})],
  (Alice) => {
    Alice.only(() => {
      assume(UInt.max >= 999999999);
      const fx0_2     = +0.2;
      const fx0_234   = +0.234;
      const fx0_5     = +0.5;
      const fx1_234   = +1.234;
      const fx1_403   = +1.403;
      const fxn4_123  = -4.123;
      const fxn4_2    = -4.2;
      const fx5_45    = +5.45;
      const fx5_8     = +5.8;
      const fx12_345  = +12.345;
      const fx28_006  = +28.006;
      const fx33_075  = +33.075;
      const fx34_56   = +34.56;
      const fx45_42   = +45.42;
      const fxn45_42  = -45.42;
      const fx57_765  = +57.765;
      const fx195_112 = +195.112;
      const fx560_70990  = +560.70990;
      const fxn560_70990 = -560.70990;
      assert(fxle(fx45_42, fx57_765));
      assert(fxle(fx45_42, fx45_42));
      assert(fxlt(fxn4_2, fxn4_123));
      assert(fxgt(fx560_70990, fx57_765));
      assert(fxeq(fx560_70990, fx560_70990));
      assert(fxne(fx560_70990, fx57_765));
      assert(fxadd(fx12_345, fx45_42) == fx57_765);
      assert(fxsub(fx45_42, fx12_345) == fx33_075);
      assert(fxmul(fx12_345, fx45_42) == fx560_70990);
      assert(fxmul(fx12_345, fxn45_42) == fxn560_70990);
      assert(fxdiv(fx34_56, fx1_234, 10000) == fx28_006);
      assert(fxeq(fxsqrt(fx34_56, 10), fx5_8));
      assert(fxeq(fxpowui(fx5_8, 3, 10), fx195_112));
      assert(fxfloor(fx1_234) == +1);
      assert(fxfloor(fxn45_42) == -46);
      assert(fxeq(fxmod(fx1_234, fx0_5), fx0_234));
      assert(fxeq(fxpow(fx5_45, fx0_2, 10, 1000), fx1_403));
      assert(fxeq(+(+2.1), - (- 2.1)));
      assert(fxeq(2.1, - (- 2.1)));
    });
  }
  );
