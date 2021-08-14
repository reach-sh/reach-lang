'reach 0.1';

export const main = Reach.App(() => {
  const Admin = Participant('Admin', {
    meta: Bytes(256),
    approve: Fun([Address], Bool),
  });
  const Scout = ParticipantClass('Scout', {
    request: Fun([], Bool),
    notify: Fun([Address, Bool], Null),
  });
  deploy();

  Admin.only(() => {
    const meta = declassify(interact.meta); });
  Admin.publish(meta);
  // XXX expose meta as a view

  const holdersM = new Set();
  // XXX expose holdersM.member as a view

  var [] = [];
  invariant( balance() == 0 );
  while ( true ) {
    commit();

    // XXX Right now, the scouts and admin interact in a synchronized manner.
    // Instead, scouts could be free to add themselves to the "wantM" set (if
    // they aren't in holdersM) and the admin can come in later and move things
    // from "wantM" to "holdersM"

    Scout.only(() => {
      const want = declassify(interact.request());
      assume(! holdersM.member(this));
    });
    Scout.publish().when(want).timeout(false);
    const requester = this;
    require(! holdersM.member(requester));
    commit();

    Admin.only(() => {
      const okay = declassify(interact.approve(requester));
    });
    Admin.publish(okay);
    if ( okay ) {
      holdersM.insert(requester);
    }
    Scout.interact.notify(requester, okay);
    continue;
  }
  commit();

  exit();
});
