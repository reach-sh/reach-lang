'reach 0.1';

const f = (x, y) => x + y;

export const main = Reach.App(
  {},
  [],
  () => {},
); // <-- reachc: "RightParenToken {tokenSpan = TokenPn 97 9 1, tokenComment = [WhiteSpace (TokenPn 96 8 12) \"\\n\"]}"
// It doesn't parse the function call where the final arg has a trailing comma

