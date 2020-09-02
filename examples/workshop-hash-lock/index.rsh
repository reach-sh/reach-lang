'reach 0.1';

export const main = Reach.App(
  { deployMode: 'firstMsg' },
  [['Alice', { amt : UInt256,
               pass: UInt256 }],
   ['Bob', { getPass: Fun([UInt256], UInt256) }] ],
  (Alice, Bob) => {
    Alice.only(() => {
      const [ amt, hpass ] =
            declassify([ interact.amt,
                         digest(interact.pass) ]); });
    Alice.publish(hpass, amt)
      .pay(amt);
    commit();

    unknowable(Bob, Alice(interact.pass));
    Bob.only(() => {
      const pass = declassify(interact.getPass(hpass));
      assume( hpass == digest(pass) ); });
    Bob.publish(pass);
    require( hpass == digest(pass) );
    transfer(amt).to(Bob);
    commit();

    exit(); } );
