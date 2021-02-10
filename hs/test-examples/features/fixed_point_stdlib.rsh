'reach 0.1';

export const main = Reach.App(
  {},
  [['Alice', {}]],
  (Alice) => {
    const fx10      = fx(10);   // 1 decimal places
    const fx100     = fx(100);  // 2 decimal places
    const fx1000    = fx(1000); // ...
    const fx100000  = fx(100000);
    Alice.only(() => {
      assume(UInt.max >= 999999999);
      const fx0_2     = fx100(20);
      const fx0_234   = fx1000(234);
      const fx0_5     = fx10(05);
      const fx1_234   = fx1000(1234);
      const fx1_403   = fx1000(1403);
      const fx5_45    = fx100(545);
      const fx5_8     = fx10(58);
      const fx12_345  = fx1000(12345);
      const fx28_006  = fx1000(28006);
      const fx33_075  = fx1000(33075);
      const fx34_56   = fx100(3456);
      const fx45_42   = fx100(4542);
      const fx57_765  = fx1000(57765);
      const fx195_112 = fx1000(195112);
      const fx560_70990 = fx100000(56070990);
      assert(fxle(fx45_42, fx57_765));
      assert(fxle(fx45_42, fx45_42));
      assert(fxgt(fx560_70990, fx57_765));
      assert(fxeq(fx560_70990, fx560_70990));
      assert(fxne(fx560_70990, fx57_765));
      assert(fxadd(fx12_345, fx45_42) == fx57_765);
      assert(fxsub(fx45_42, fx12_345) == fx33_075);
      assert(fxmul(fx12_345, fx45_42) == fx560_70990);
      assert(fxdiv(fx34_56, fx1_234, 10000) == fx28_006);
      assert(fxsqrt(fx34_56, 10) == fx5_8);
      assert(fxpowi(fx5_8, 3, 10) == fx195_112);
      assert(fxfloor(fx1_234) == 1);
      assert(fxeq(fxmod(fx1_234, fx0_5), fx0_234));
      assert(fxeq(fxpow(fx5_45, fx0_2, 10, 1000), fx1_403));
    });
  }
  );
