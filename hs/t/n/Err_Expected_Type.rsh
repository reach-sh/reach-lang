'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', { ...hasConsoleLogger });
  deploy();

  const o = Data({
    Some: Array,
    None: Null,
  });

});
