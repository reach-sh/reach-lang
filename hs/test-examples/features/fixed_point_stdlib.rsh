'reach 0.1';

export const main = Reach.App(
  {},
  [['Alice', {}]],
  (Alice) => {
    const fp10      = mk_fixed_point(10);   // 1 decimal places
    const fp100     = mk_fixed_point(100);  // 2 decimal places
    const fp1000    = mk_fixed_point(1000); // ...
    const fp100000  = mk_fixed_point(100000);
    Alice.only(() => {
      assume(UInt.max >= 999999999);
      const fp1_234   = fp1000(1234);
      const fp5_8     = fp10(58);
      const fp12_345  = fp1000(12345);
      const fp28_006  = fp1000(28006);
      const fp33_075  = fp1000(33075);
      const fp34_56   = fp100(3456);
      const fp45_42   = fp100(4542);
      const fp57_765  = fp1000(57765);
      const fp560_70990 = fp100000(56070990);
      assert(fixed_point_cmp(le, fp45_42, fp57_765));
      assert(fixed_point_cmp(le, fp45_42, fp45_42));
      assert(fixed_point_cmp(gt, fp560_70990, fp57_765));
      assert(fixed_point_add(fp12_345, fp45_42) == fp57_765);
      assert(fixed_point_sub(fp45_42, fp12_345) == fp33_075);
      assert(fixed_point_mul(fp12_345, fp45_42) == fp560_70990);
      assert(fixed_point_div(fp34_56, fp1_234, 10000) == fp28_006);
      assert(fixed_point_sqrt(fp34_56, 10) == fp5_8);
    });
  }
  );
