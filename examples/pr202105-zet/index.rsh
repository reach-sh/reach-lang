'reach 0.1';
'use strict';

const Command = Data({
  Get: Null,
  Set: UInt,
});

const IUser = {
  what: Fun([], Command),
  get: Fun([UInt], Null),
  set: Fun([UInt], Null),
};

export const main = Reach.App(
  {},
  [ Participant("User", IUser) ],
  (User) => {
    User.publish();

    var val = 0;
    invariant( balance() == 0 );
    while ( true ) {
      commit();

      User.only(() => {
        const cmd = declassify(interact.what()); });
      User.publish(cmd);
      switch ( cmd ) {
        case Get: {
          User.only(() => interact.get(val));
          continue; }
        case Set: {
          User.only(() => interact.set(cmd));
          val = cmd;
          continue; }
      } }
    commit();
    assert(false);
    exit();
  });

