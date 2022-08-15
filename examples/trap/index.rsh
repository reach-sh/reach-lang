'reach 0.1';

const f = (b, x) => {
  return b ? x - 5 : x;
}

export const trap = is(f, Fun([Bool, UInt], UInt));
