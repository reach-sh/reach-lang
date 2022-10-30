// log-attack1
pragma abicoder v2;
pragma solidity ^0.8.0;

struct T5 {
  address payable v76;
  uint256 v77;
  }
struct T6 {
  uint256 time;
  T5 msg;
  }

contract LogAttack1 {
  constructor () payable {
  }

  event _reach_e1(address _who, T6 _a);
  function m1(address payable acct, uint256 amt) external payable {
    T6 memory a;
    a.time = 0;
    a.msg.v76 = acct;
    a.msg.v77 = amt + 1;
    emit _reach_e1(msg.sender, a);
  }
}
