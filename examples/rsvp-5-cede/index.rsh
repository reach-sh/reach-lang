'reach 0.1';
'use strict';

const Details = Object({
  name: Bytes(128),
  reservation: UInt,
  deadline: UInt,
  host: Address,
});

export const main = Reach.App(() => {
  const Admin = Participant('Admin', {
    details: Details,
    launched: Fun([Contract], Null),
  });
  const Guest = API('Guest', {
    register: Fun([], Null),
  });
  const Host = API('Host', {
    checkin: Fun([Address, Bool], Null),
  });
  init();

  Admin.only(() => {
    const details = declassify(interact.details);
  });
  Admin.publish(details);
  const { reservation, deadline, host } = details;
  enforce( thisConsensusTime() < deadline, "too late" );
  Admin.interact.launched(getContract());

  const Guests = new Map(Bool);
  const [ done, howMany ] =
    parallelReduce([ false, 0 ])
    .invariant( Guests.size() == howMany, "howMany accurate" )
    .invariant( balance() == howMany * reservation, "balance accurate" )
    .while( ! ( done && howMany == 0 ) )
    .api_(Guest.register, () => {
      check(! done, "event started");
      check(isNone(Guests[this]), "already registered");
      return [ reservation, (ret) => {
        enforce( thisConsensusTime() < deadline, "too late" );
        Guests[this] = true;
        ret(null);
        return [ false, howMany + 1 ];
      } ];
    })
    .api_(Host.checkin, (guest, showed) => {
      check(this == host, "not the host");
      check(isSome(Guests[guest]), "no reservation");
      return [ 0, (ret) => {
        enforce( thisConsensusTime() >= deadline, "too early" );
        delete Guests[guest];
        transfer(reservation).to(showed ? guest : host);
        ret(null);
        return [ true, howMany - 1 ];
      } ];
    });
  commit();

  exit();
});
