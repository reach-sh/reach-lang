'reach 0.1';

const fooFx = fx(1)(Pos, 3);
const barFx = fx(1)(Pos, 2);
const zeroFx = fx(1)(Pos, 0);
check(fooFx + barFx > zeroFx, 'Foo+Bar must be positive');
