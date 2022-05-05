// log-attack1
pragma abicoder v2;
pragma solidity ^0.8.0;

struct T4 {
  address payable v75;
  uint256 v76;
  }
struct T5 {
  uint256 time;
  T4 msg;
  }

contract LogAttack1 {
  constructor () payable {
  }

  event _reach_e1(address _who, T5 _a);
  function m1(address payable acct, uint256 amt) external payable {
    T5 memory a;
    a.time = 0;
    a.msg.v75 = acct;
    a.msg.v76 = amt + 1;
    emit _reach_e1(msg.sender, a);
  }
}
