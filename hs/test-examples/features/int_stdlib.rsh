'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      A.only(() => {
        const r1 = iadd( int(Positive, 2) , int(Positive, 4) );
        const r2 = iadd( int(Negative, 2) , int(Negative, 4) );
        const r3 = iadd( int(Negative, 2) , int(Positive, 4) );
        const r4 = iadd( int(Positive, 2) , int(Negative, 4) );
        assert(r1 == int(Positive, 6));
        assert(r2 == int(Negative, 6));
        assert(r3 == int(Positive, 2));
        assert(r4 == int(Negative, 2));

        const s1 = isub( int(Positive, 2) , int(Positive, 4) );
        const s2 = isub( int(Negative, 2) , int(Negative, 4) );
        const s3 = isub( int(Negative, 2) , int(Positive, 4) );
        const s4 = isub( int(Positive, 2) , int(Negative, 4) );
        assert(s1 == int(Negative, 2));
        assert(s2 == int(Positive, 2));
        assert(s3 == int(Negative, 6));
        assert(s4 == int(Positive, 6));

        const m1 = imul( int(Positive, 2) , int(Positive, 4) );
        const m2 = imul( int(Negative, 2) , int(Negative, 4) );
        const m3 = imul( int(Negative, 2) , int(Positive, 4) );
        const m4 = imul( int(Positive, 2) , int(Negative, 4) );
        assert(m1 == int(Positive, 8));
        assert(m2 == int(Positive, 8));
        assert(m3 == int(Negative, 8));
        assert(m4 == int(Negative, 8));

        const d1 = idiv( int(Positive, 4), int(Positive, 2) );
        const d2 = idiv( int(Negative, 4), int(Negative, 2) );
        const d3 = idiv( int(Positive, 4), int(Negative, 2) );
        const d4 = idiv( int(Negative, 4), int(Positive, 2) );
        assert(d1 == int(Positive, 2));
        assert(d2 == int(Positive, 2));
        assert(d3 == int(Negative, 2));
        assert(d4 == int(Negative, 2));

        const mo1 = imod( int(Positive, 4), int(Positive, 3) );
        const mo2 = imod( int(Negative, 4), int(Negative, 3) );
        const mo3 = imod( int(Positive, 4), int(Negative, 3) );
        const mo4 = imod( int(Negative, 4), int(Positive, 3) );
        assert(mo1 == int(Positive, 1));
        assert(mo2 == int(Negative, 1));
        assert(mo3 == int(Positive, 1));
        assert(mo4 == int(Negative, 1));

        const gt1 = igt( int(Positive, 4), int(Positive, 3) );
        const gt2 = igt( int(Negative, 4), int(Negative, 3) );
        const gt3 = igt( int(Positive, 4), int(Negative, 3) );
        const gt4 = igt( int(Negative, 4), int(Positive, 3) );
        assert(gt1 == true);
        assert(gt2 == false);
        assert(gt3 == true);
        assert(gt4 == false);

        const ge1 = ige( int(Positive, 4), int(Positive, 4) );
        const ge2 = ige( int(Negative, 4), int(Negative, 4) );
        const ge3 = ige( int(Positive, 4), int(Negative, 4) );
        const ge4 = ige( int(Negative, 4), int(Positive, 4) );
        assert(ge1 == true);
        assert(ge2 == true);
        assert(ge3 == true);
        assert(ge4 == false);

        const lt1 = ilt( int(Positive, 4), int(Positive, 3) );
        const lt2 = ilt( int(Negative, 4), int(Negative, 3) );
        const lt3 = ilt( int(Positive, 4), int(Negative, 3) );
        const lt4 = ilt( int(Negative, 4), int(Positive, 3) );
        assert(lt1 == false);
        assert(lt2 == true);
        assert(lt3 == false)
        assert(lt4 == true);

        const le1 = ile( int(Positive, 4), int(Positive, 4) );
        const le2 = ile( int(Negative, 4), int(Negative, 4) );
        const le3 = ile( int(Positive, 4), int(Negative, 4) );
        const le4 = ile( int(Negative, 4), int(Positive, 4) );
        assert(le1 == true);
        assert(le2 == true);
        assert(le3 == false);
        assert(le4 == true);
      });
    }
  );
