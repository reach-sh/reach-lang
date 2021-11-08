'reach 0.1';
'use strict';

export const main = Reach.App(
  { },
  [Participant('Alice', { amt : UInt,
               pass: UInt }),
   Participant('Bob', { ...hasConsoleLogger, getPass: Fun([], UInt) }) ],
  (Alice, Bob) => {
    Alice.only(() => {
      const _pass = interact.pass;
      const [ amt, passDigest ] =
            declassify([ interact.amt,
                         digest(_pass) ]); });
    Alice.publish(passDigest, amt)
      .pay(amt);
    commit();

    unknowable(Bob, Alice(_pass));
    Bob.only(() => {
      const pass = declassify(interact.getPass());
      interact.log([passDigest, pass, digest(pass)]);
      assume( passDigest == digest(pass) ); });
    Bob.publish(pass);
    require( passDigest == digest(pass) );
    transfer(amt).to(Bob);
    commit();

    exit(); } );
