'reach 0.1';
'use strict';

export const main =
  Reach.App(
    {},
    [Participant('Alice', {myAddr: Fun([Address], Null)})],
    (Alice) => {
      const abstractedSet = (Who) => {
        Who.only(() => {
          const them = Who; });
        Who.publish(them);
        require(Who == them);
        commit();
        return them; };

      const them =
        abstractedSet(Alice);

      Alice.only(() => {
        const me = Alice; });
      Alice.publish(me);
      require(Alice == me);
      assert(them == me);
      commit();

      const abstractedCheck = (Who) =>
        Who.only(() => {
          interact.myAddr(me);
          assert(me == Who); });
      abstractedCheck(Alice);

      exit(); });
