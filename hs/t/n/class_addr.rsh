'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Constructor', {}),
     ParticipantClass('C', {})],
    (Constructor, C) => {
      Constructor.publish();
      commit();

      C.only(() => {
        const addr1 = this; });
      C.publish(addr1);
      commit();

      C.only(() => {
        const addr2 = this; });
      C.publish(addr2);
      require(addr1 == addr2);
      commit();
      exit();
    });

