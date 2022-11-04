pragma abicoder v2;

pragma solidity ^0.8.0;

struct T4 {
  uint256 elem0;
  }

contract WeirdContract {
  uint256 _x;
  constructor() payable {
    _x = 0;
  }

  event _reach_e1(address _who, T4 _a);
  function f(uint256 x) external payable {
    T4 memory a;
    a.elem0 = 0;
    emit _reach_e1(msg.sender, a);
    _x = x;
  }
}
