'reach 0.1';

const testIntervalEq = () => {
  assert(intervalEq( intervalOO(+1, +1), intervalOO(+1, +1) ));
  assert(intervalEq( intervalCC(+1, +1), intervalCC(+1, +1) ));
}

const testIntervalNeq = () => {
  assert(intervalNeq( intervalOO(+1, +2), intervalOO(+4, +5) ));
  assert(intervalNeq( intervalCC(+1, +2), intervalCC(+4, +5) ));
}

const testIntervalLt = () => {
  assert(intervalLt( intervalOO(+1, +2), intervalOO(+3, +5) ));
  assert(intervalLt( intervalCC(+1, +2), intervalCC(+3, +5) ));
}

const testIntervalLte = () => {
  assert(intervalLte( intervalOO(+3, +4), intervalOO(+5, +7) ));
  assert(intervalLte( intervalCC(+1, +2), intervalCC(+1, +2) ));
}

const testIntervalGt = () => {
  assert(intervalGt( intervalOO(+5, +7), intervalOO(+3, +4) ));
  assert(intervalGt( intervalCC(+5, +7), intervalCC(+3, +4) ));
}

const testIntervalGte = () => {
  assert(intervalGte( intervalOO(+5, +7), intervalOO(+4, +6) ));
  assert(intervalGte( intervalCC(+2, +2), intervalCC(+2, +2) ));
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      testIntervalEq();
      testIntervalNeq();
      testIntervalLt();
      testIntervalLte();
      testIntervalGt();
      testIntervalGte();
    });
