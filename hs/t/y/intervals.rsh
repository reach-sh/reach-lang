'reach 0.1';

const testIntervalEq = () => {
  assert(intervalEq( intervalOO(+1, +1), intervalOO(+1, +1) ));
  assert(intervalEq( intervalCC(+1, +1), intervalCC(+1, +1) ));
}

const testIntervalLt = () => {
  assert(intervalLt( intervalOO(+1, +2), intervalOO(+3, +5) ));
  assert(intervalLt( intervalCC(+1, +2), intervalCC(+3, +5) ));
}

const testIntersect = () => {
  const i1 = intervalOO(+3, +11);
  const i2 = intervalCC(+7, +9);
  const ii = intervalCO(+7, +11);
  assert(intervalIntersection(i1, i2) == ii);
}

const testUnion = () => {
  const i1 = intervalOO(+3, +9);
  const i2 = intervalCC(+7, +11);
  const iu = intervalOC(+3, +11);
  assert(intervalUnion(i1, i2) == iu);
}

const testWidth = () => {
  const i = intervalOO(+4, +45);
  assert(intervalWidth(i) == +41);
}

const testAbs = () => {
  const i = intervalCC(+1, +10);
  assert(intervalAbs(i) == +10);
}

const testAdd = () => {
  const i1 = intervalCC(+3, +1);
  const i2 = intervalCC(+43, +14);
  const ex = intervalCC(+46, +15);
  assert(intervalAdd(i1, i2) == ex);
}

const testSub = () => {
  const i1 = intervalCC(+30, +18);
  const i2 = intervalCC(+15, +4);
  const ex = intervalCC(+26, +3);
  assert(intervalSub(i1, i2) == ex);
}

const testMul = () => {
  const i1 = intervalCC(+2, +4);
  const i2 = intervalCC(+8, +12);
  const ex = intervalCC(+16, +48);
  assert(intervalMul(i1, i2) == ex);
}

const testDiv = () => {
  const i1 = intervalCC(+24, +12);
  const i2 = intervalCC(+6, +3);
  const ex = intervalCC(+2, +8);
  assert(intervalDiv(i1, i2) == ex);
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      testIntervalEq();
      testIntervalLt();
      testIntersect();
      testUnion();
      testWidth();
      testAbs();
      testAdd();
      testSub();
      testMul();
      testDiv();
    });
