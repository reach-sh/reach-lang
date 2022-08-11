'reach 0.1';
'use strict';

const Details = Object({
  name: Bytes(128),
  reservation: UInt,
  deadline: UInt,
  host: Address,
});

export const main = Reach.App(() => {
  const Host = Participant('Host', {
    details: Details,
    showed: Bool,
  });
  const Guest = Participant('Guest', {
    details: Details,
    registered: Fun([Contract], Null),
  });
  init();

  Guest.only(() => {
    const details = declassify(interact.details);
  });
  Guest.publish(details).pay(details.reservation);
  const { reservation, deadline, host } = details;
  enforce( thisConsensusTime() < deadline, "too late" );
  Guest.interact.registered(getContract());
  commit();

  Host.only(() => {
    const hdetails = declassify(interact.details);
    check( details == hdetails, "wrong event" );
    const showed = declassify(interact.showed);
  });
  Host.publish(showed).check(() => {
    check(this == host, "not the host");
  });
  enforce( thisConsensusTime() >= deadline, "too early" );
  transfer(reservation).to(showed ? Guest : Host);
  commit();

  exit();
});
