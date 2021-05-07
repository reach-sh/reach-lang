'reach 0.1';

const makeApp = (f) =>
  Reach.App({}, [Participant('Alice', { f: Fun([UInt], Null) })], (A) => f(A));

export const one = makeApp((A) => {
  A.only(() => {
    interact.f(1); }); });

export const two = makeApp((A) => {
  A.only(() => {
    interact.f(2); }); });
