'reach 0.1';

export const main = Reach.App(
  {},
  [],
  () => {
    wait(relativeTime(1));
    exit(); } );
