'reach 0.1';

export const main = Reach.App(() => {
  const A = View('A', {
    x: UInt
  }, {
    x: ['y', 'z']
  });
  const B = View('B', {
    xp: UInt
  }, {
    xp: ['y']
  })
  init();

});
