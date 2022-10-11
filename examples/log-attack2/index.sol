// log-attack2
pragma abicoder v2;
pragma solidity ^0.8.0;

struct T7 {
  uint256 v99;
  }
struct T8 {
  uint256 time;
  T7 msg;
  }

contract LogAttack2  {
  constructor () payable {
  }

  event _reach_e2(address _who, T8 _a);
  function m2() external payable {
    T8 memory a;
    a.time = 0;
    a.msg.v99 = 1337;
    emit _reach_e2(msg.sender, a);
  }
}
