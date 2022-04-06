'reach 0.1';

const Msg = Bytes(256);
const CommonI = {
  hear: Fun([Address, Msg], Null) };
const UserI = {
  ...CommonI,
  make: Fun([], Tuple(Bool, Msg)) };
const ManagerI = {
  ...CommonI,
  fee: UInt };

export const main = Reach.App(() => {
  setOptions({ untrustworthyMaps: true });
  const Manager = Participant('Manager', ManagerI);
  const User = ParticipantClass('User', UserI);
  init();
    Manager.only(() => {
      const fee = declassify(interact.fee); });
    Manager.publish(fee);

    const RecordM = new Map(Address, Msg);
    var [] = [];
    invariant( balance() == 0 );
    while ( true ) {
      commit();

      User.only(() => {
        const [ haveMsg, msg ] = declassify(interact.make());
        const when = isNone(RecordM[this]) && haveMsg; });
      User.publish(msg).when(when).pay(fee).timeout(false);
      require(isNone(RecordM[this]));
      transfer(fee).to(Manager);
      RecordM[this] = msg;
      each([User, Manager], () => {
        interact.hear(this, msg); });
      continue;
    }

    commit();
    assert(false);
    exit();
  }
);

