'reach 0.1';

assert( ({a:1}["a"]) === 1 );

export const main = Reach.App(() => {
  const A = Participant('A', {});
  deploy();
  exit();
});

