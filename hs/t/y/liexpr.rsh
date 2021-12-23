'reach 0.1';

export const main = Reach.App(() => {
  const aA = [ Participant('A', { ...hasConsoleLogger }) ];
  init();
  aA[0].interact.log('Test');
  exit();
});
