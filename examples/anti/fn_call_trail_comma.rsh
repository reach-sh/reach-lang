'reach 0.1';

const f = (x, y) => x + y;

export const main = Reach.App(
  {},
  [],
  () => {
    const z = f(
      1,
      2,
    );
  }
); // <-- reachc: "RightParenToken {tokenSpan = TokenPn 134 12 5,
// tokenComment = [WhiteSpace (TokenPn 129 11 9) \"\\n\"]}"
// It doesn't parse the function call where the final arg has a trailing comma
