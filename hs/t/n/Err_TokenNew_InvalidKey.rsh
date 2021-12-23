'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    tok: Token,
  });
  init();
  A.publish();
  const tok = new Token({coolness: true});
  commit();
  exit();
});
