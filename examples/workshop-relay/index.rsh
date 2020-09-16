'reach 0.1';

export const main = Reach.App(
  { deployMode: 'firstMsg' },
  [['Alice', { amt : UInt256,
               getRelay: Fun([], Address) }],
   ['Relay', { getBob: Fun([], Address) }] ],
  (Alice, Relay) => {
    Alice.only(() => {
      const [ amt, relay ] =
            declassify([ interact.amt,
                         interact.getRelay()]); });
    Alice.publish(amt, relay)
      .pay(amt);
    Relay.set(relay);
    commit();

    Relay.only(() => {
      const bob = declassify(interact.getBob()); });
    Relay.publish(bob);
    transfer(amt).to(bob);
    commit();

    exit(); } );
