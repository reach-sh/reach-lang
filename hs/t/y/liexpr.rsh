'reach 0.1';

export const main = Reach.App(() => {
  const aA = [ Participant('A', { ...hasConsoleLogger }) ];
  deploy();
  aA[0].interact.log('Test');
  exit();
});
