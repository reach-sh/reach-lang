'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    if ("truthy") {
      return 1;
    } else {
      return 0;
    }
  }
);
