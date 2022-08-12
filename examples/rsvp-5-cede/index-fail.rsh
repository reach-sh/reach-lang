'reach 0.1';
'use strict';

const Details = Object({
  name: Bytes(128),
  reservation: UInt,
  deadline: UInt,
});

export const main = Reach.App(() => {
  const Host = Participant('Host', {
    details: Details,
    launched: Fun([Contract], Null),
  });
  const GuestP = API('GuestP', {
    register: Fun([], Null),
  });
  const HostP = API('HostP', {
    checkin: Fun([Address, Bool], Null),
  });
  init();

  Host.only(() => {
    const details = declassify(interact.details);
  });
  Host.publish(details);
  const { reservation, deadline } = details;
  const host = this;
  enforce( thisConsensusTime() < deadline, "too late" );
  Host.interact.launched(getContract());

  const Guests = new Map(Bool);
  const [ done, howMany ] =
    parallelReduce([ false, 0 ])
    .invariant( Guests.size() == howMany, "howMany accurate" )
    .invariant( balance() == howMany * reservation, "balance accurate" )
    .while( ! ( done && howMany == 0 ) )
    .api_(GuestP.register, () => {
      check(! done, "event started");
      check(isNone(Guests[this]), "already registered");
      return [ reservation, (ret) => {
        enforce( thisConsensusTime() < deadline, "too late" );
        Guests[this] = true;
        ret(null);
        return [ false, howMany + 1 ];
      } ];
    })
    .api_(HostP.checkin, (guest, showed) => {
      check(this == host, "not the host");
      check(isSome(Guests[guest]), "no reservation");
      return [ 0, (ret) => {
        enforce( thisConsensusTime() >= deadline, "too early" );
        //delete Guests[guest];
        transfer(reservation).to(showed ? guest : Host);
        ret(null);
        return [ true, howMany - 1 ];
      } ];
    });
  commit();

  exit();
});
