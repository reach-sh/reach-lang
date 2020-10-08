contract Stdlib {
  function safeAdd(uint x, uint y) internal pure returns (uint z) {
    require((z = x + y) >= x, "add overflow"); }
  function safeSub(uint x, uint y) internal pure returns (uint z) {
    require((z = x - y) <= x, "sub wraparound"); }
  function safeMul(uint x, uint y) internal pure returns (uint z) {
    require(y == 0 || (z = x * y) / y == x, "mul overflow"); }
}
