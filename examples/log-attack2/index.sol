// log-attack2
pragma abicoder v2;
pragma solidity ^0.8.0;

struct T5 {
  uint256 elem0;
  uint256 elem1;
  }

contract LogAttack2  {
  constructor () payable {
  }

  event _reach_e2(address _who, T5 _a);
  function m2() external payable {
    T5 memory a;
    a.elem0 = 0;
    a.elem1 = 1337;
    emit _reach_e2(msg.sender, a);
  }
}
