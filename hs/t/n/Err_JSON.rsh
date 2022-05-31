'reach 0.1';
export const main = Reach.App(() => {
  setOptions({ connectors: [ ETH ] });
  const A = Participant('A', {});
  const CC = ContractCode({ ETH: Participant });
  init();
  exit();
});
