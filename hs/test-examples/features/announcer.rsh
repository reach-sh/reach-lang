'reach 0.1';

const Msg = Bytes(256);
const CommonI = {
  hear: Fun([Msg], Null) };
const UserI = {
  ...CommonI,
  make: Fun([], Tuple(Bool, Msg)) };
const ManagerI = {
  ...CommonI,
  fee: UInt };

export const main = Reach.App(
  {},
  [ Participant('Manager', ManagerI),
    ParticipantClass('User', UserI),
  ],
  (Manager, User) => {
    Manager.only(() => {
      const fee = declassify(interact.fee); });
    Manager.publish(fee);

    var [] = [];
    invariant( balance() == 0 );
    while ( true ) {
      commit();

      User.only(() => {
        const [ when, msg ] = declassify(interact.make()); });
      User.publish(msg).when(when).pay(fee).timeout(false);
      transfer(fee).to(Manager);
      each([User, Manager], () => {
        interact.hear(msg); });
      continue;
    }

    commit();
    assert(false);
    exit();
  }
);
