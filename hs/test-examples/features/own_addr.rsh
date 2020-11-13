'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}]],
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
          assert(me == Who); });
      abstractedCheck(Alice);

      exit(); });
