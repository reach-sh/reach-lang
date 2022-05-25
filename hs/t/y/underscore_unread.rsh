'reach 0.1';

export const main = Reach.App(() => {
    const A1 = Participant('Alice', {});
    const [x, _] = [1, "ignored"];
    const y = x;
	init();
  A1.publish()
  commit()
  }
);
