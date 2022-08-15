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
  const Info = View('Info', {
    details: Details,
    howMany: UInt,
    reserved: Fun([Address], Bool),
  });
  const Notify = Events({
    register: [Address],
    checkin: [Address, Bool],
  });
  init();

  Admin.only(() => {
    const details = declassify(interact.details);
  });
  Admin.publish(details);
  const { reservation, deadline, host } = details;
  enforce( thisConsensusTime() < deadline, "too late" );
  Admin.interact.launched(getContract());
  Info.details.set(details);

  const Guests = new Map(Bool);
  Info.reserved.set((who) => isSome(Guests[who]));
  const [ done, howMany ] =
    parallelReduce([ false, 0 ])
    .define(() => {
      Info.howMany.set(howMany);
    })
    .invariant( Guests.size() == howMany, "howMany accurate" )
    .invariant( balance() == howMany * reservation, "balance accurate" )
    .while( ! ( done && howMany == 0 ) )
    .api_(Guest.register, () => {
      check(! done, "event started");
      check(isNone(Guests[this]), "already registered");
      return [ reservation, (ret) => {
        enforce( thisConsensusTime() < deadline, "too late" );
        Guests[this] = true;
        Notify.register(this);
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
        Notify.checkin(guest, showed);
        ret(null);
        return [ true, howMany - 1 ];
      } ];
    });
  commit();

  exit();
});
