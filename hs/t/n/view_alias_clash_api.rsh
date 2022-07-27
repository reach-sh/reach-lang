'reach 0.1';

export const main = Reach.App(() => {
  const D = Participant('D', { x: Bool });
  const A = API('A', { y: Fun([UInt], Null) });
  const V = View({ x: UInt }, { x: ['A_y']});
  init();
  D.publish();
  commit();
});
