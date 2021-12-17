pragma abicoder v2;

pragma solidity ^0.8.0;

struct T5 {
  uint256 time;
  bool msg;
  }

contract WeirdContract {
  uint256 _x;
  constructor() payable {
    _x = 0;
  }

  event _reach_e1(T5 _a);
  function f(uint256 x) external payable {
    T5 memory a;
    a.time = 0;
    a.msg = true;
    emit _reach_e1(a);
    _x = x;
  }
}
