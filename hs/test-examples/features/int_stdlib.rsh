'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {}]],
    (A) => {
      A.only(() => {
        const r1 = iadd(+2 , +4);
        const r2 = iadd(-2 , -4);
        const r3 = iadd(-2 , +4);
        const r4 = iadd(+2 , -4);
        assert(r1 == +6);
        assert(r2 == -6);
        assert(r3 == +2);
        assert(r4 == -2);

        const s1 = isub(+2 , +4);
        const s2 = isub(-2 , -4);
        const s3 = isub(-2 , +4);
        const s4 = isub(+2 , -4);
        assert(s1 == -2);
        assert(s2 == +2);
        assert(s3 == -6);
        assert(s4 == +6);

        const m1 = imul(+2 , +4);
        const m2 = imul(-2 , -4);
        const m3 = imul(-2 , +4);
        const m4 = imul(+2 , -4);
        assert(m1 == +8);
        assert(m2 == +8);
        assert(m3 == -8);
        assert(m4 == -8);

        const d1 = idiv(+4, +2);
        const d2 = idiv(-4, -2);
        const d3 = idiv(+4, -2);
        const d4 = idiv(-4, +2);
        assert(d1 == +2);
        assert(d2 == +2);
        assert(d3 == -2);
        assert(d4 == -2);

        const mo1 = imod(+4, +3);
        const mo2 = imod(-4, -3);
        const mo3 = imod(+4, -3);
        const mo4 = imod(-4, +3);
        assert(mo1 == +1);
        assert(mo2 == -1);
        assert(mo3 == +1);
        assert(mo4 == -1);

        const gt1 = igt(+4, +3);
        const gt2 = igt(-4, -3);
        const gt3 = igt(+4, -3);
        const gt4 = igt(-4, +3);
        assert(gt1 == true);
        assert(gt2 == false);
        assert(gt3 == true);
        assert(gt4 == false);

        const ge1 = ige(+4, +4);
        const ge2 = ige(-4, -4);
        const ge3 = ige(+4, -4);
        const ge4 = ige(-4, +4);
        assert(ge1 == true);
        assert(ge2 == true);
        assert(ge3 == true);
        assert(ge4 == false);

        const lt1 = ilt(+4, +3);
        const lt2 = ilt(-4, -3);
        const lt3 = ilt(+4, -3);
        const lt4 = ilt(-4, +3);
        assert(lt1 == false);
        assert(lt2 == true);
        assert(lt3 == false)
        assert(lt4 == true);

        const le1 = ile(+4, +4);
        const le2 = ile(-4, -4);
        const le3 = ile(+4, -4);
        const le4 = ile(-4, +4);
        assert(le1 == true);
        assert(le2 == true);
        assert(le3 == false);
        assert(le4 == true);

        assert(- (- 4) == + (+4));
        const x = - 5;
        assert(-x == +5);
        const o = { u: 5, i: +5 };
        assert(+o.u == -(-o.u));
        assert(+o.i == o.i);
      });
    }
 );
