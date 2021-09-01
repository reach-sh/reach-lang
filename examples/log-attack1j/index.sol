pragma abicoder v2;

pragma solidity ^0.8.0;

struct T3 {
  address payable v62;
  uint256 v63;
}
struct T4 {
  bool svs;
  T3 msg;
}

contract WeirdContract {
  uint256 _x;
  constructor() payable {
    _x = 0;
  }

  event e1(T4 _a);
  function f(uint256 x) external payable {
    T4 memory a;
    a.svs = false;
    a.msg.v62 = payable(msg.sender);
    a.msg.v63 = x + 1;
    emit e1(a);
    _x = x;
  }
}
