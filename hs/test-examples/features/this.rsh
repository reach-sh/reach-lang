'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', {}]],
    (Alice) => {
      const abstractedSet = (Who) => {
        Who.only(() => {
          const them = this; });
        Who.publish(them);
        require(this == them);
        assert(Who == them);
        commit();
        return them; };

      const them =
        abstractedSet(Alice);

      Alice.only(() => {
        const me = this; });
      Alice.publish(me);
      require(Alice == me);
      assert(this == me);
      assert(them == me);
      commit();

      const abstractedCheck = (Who) =>
        Who.only(() => {
          assert(me == Who);
          assert(me == this); });
      abstractedCheck(Alice);

      exit(); });
