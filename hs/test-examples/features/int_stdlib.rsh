'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      A.only(() => {
        const r1 = iadd( int(Pos, 2) , int(Pos, 4) );
        const r2 = iadd( int(Neg, 2) , int(Neg, 4) );
        const r3 = iadd( int(Neg, 2) , int(Pos, 4) );
        const r4 = iadd( int(Pos, 2) , int(Neg, 4) );
        assert(r1 == int(Pos, 6));
        assert(r2 == int(Neg, 6));
        assert(r3 == int(Pos, 2));
        assert(r4 == int(Neg, 2));

        const s1 = isub( int(Pos, 2) , int(Pos, 4) );
        const s2 = isub( int(Neg, 2) , int(Neg, 4) );
        const s3 = isub( int(Neg, 2) , int(Pos, 4) );
        const s4 = isub( int(Pos, 2) , int(Neg, 4) );
        assert(s1 == int(Neg, 2));
        assert(s2 == int(Pos, 2));
        assert(s3 == int(Neg, 6));
        assert(s4 == int(Pos, 6));

        const m1 = imul( int(Pos, 2) , int(Pos, 4) );
        const m2 = imul( int(Neg, 2) , int(Neg, 4) );
        const m3 = imul( int(Neg, 2) , int(Pos, 4) );
        const m4 = imul( int(Pos, 2) , int(Neg, 4) );
        assert(m1 == int(Pos, 8));
        assert(m2 == int(Pos, 8));
        assert(m3 == int(Neg, 8));
        assert(m4 == int(Neg, 8));

        const d1 = idiv( int(Pos, 4), int(Pos, 2) );
        const d2 = idiv( int(Neg, 4), int(Neg, 2) );
        const d3 = idiv( int(Pos, 4), int(Neg, 2) );
        const d4 = idiv( int(Neg, 4), int(Pos, 2) );
        assert(d1 == int(Pos, 2));
        assert(d2 == int(Pos, 2));
        assert(d3 == int(Neg, 2));
        assert(d4 == int(Neg, 2));

        const mo1 = imod( int(Pos, 4), int(Pos, 3) );
        const mo2 = imod( int(Neg, 4), int(Neg, 3) );
        const mo3 = imod( int(Pos, 4), int(Neg, 3) );
        const mo4 = imod( int(Neg, 4), int(Pos, 3) );
        assert(mo1 == int(Pos, 1));
        assert(mo2 == int(Neg, 1));
        assert(mo3 == int(Pos, 1));
        assert(mo4 == int(Neg, 1));

        const gt1 = igt( int(Pos, 4), int(Pos, 3) );
        const gt2 = igt( int(Neg, 4), int(Neg, 3) );
        const gt3 = igt( int(Pos, 4), int(Neg, 3) );
        const gt4 = igt( int(Neg, 4), int(Pos, 3) );
        assert(gt1 == true);
        assert(gt2 == false);
        assert(gt3 == true);
        assert(gt4 == false);

        const ge1 = ige( int(Pos, 4), int(Pos, 4) );
        const ge2 = ige( int(Neg, 4), int(Neg, 4) );
        const ge3 = ige( int(Pos, 4), int(Neg, 4) );
        const ge4 = ige( int(Neg, 4), int(Pos, 4) );
        assert(ge1 == true);
        assert(ge2 == true);
        assert(ge3 == true);
        assert(ge4 == false);

        const lt1 = ilt( int(Pos, 4), int(Pos, 3) );
        const lt2 = ilt( int(Neg, 4), int(Neg, 3) );
        const lt3 = ilt( int(Pos, 4), int(Neg, 3) );
        const lt4 = ilt( int(Neg, 4), int(Pos, 3) );
        assert(lt1 == false);
        assert(lt2 == true);
        assert(lt3 == false)
        assert(lt4 == true);

        const le1 = ile( int(Pos, 4), int(Pos, 4) );
        const le2 = ile( int(Neg, 4), int(Neg, 4) );
        const le3 = ile( int(Pos, 4), int(Neg, 4) );
        const le4 = ile( int(Neg, 4), int(Pos, 4) );
        assert(le1 == true);
        assert(le2 == true);
        assert(le3 == false);
        assert(le4 == true);
      });
    }
  );
