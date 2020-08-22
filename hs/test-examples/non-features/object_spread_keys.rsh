'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    const obj = {x: 1, y: 2};
    const {x, ...objY} = obj;

    // objY does have y
    const y = objY.y;

    // objY should not have x
    const z = objY.x;
  }
);
