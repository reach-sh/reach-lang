'reach 0.1';

export const main = Reach.App(
  { deployMode: 'firstMsg' },
  [],
  () => {
    wait(1);
    exit(); } );
