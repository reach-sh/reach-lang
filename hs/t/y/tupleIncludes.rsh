'reach 0.1'

const tup = [1, 2, "hi", -3];
const b1 = tup.includes("hi");
const b2 = tup.includes(2);
const b3 = tup.includes(0.1);
const b4 = Tuple.includes(tup, 3);
const b5 = Tuple.includes([1,2,3,"owl"], 3);
const b6 = [1,2,3,"fourteen"].includes(7);

