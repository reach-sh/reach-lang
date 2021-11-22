pragma abicoder v2;
pragma solidity ^0.8.0;

struct T4 {
  address payable v62;
  uint256 v63;
  }
struct T5 {
  uint256 time;
  T4 msg;
  }

contract LogAttack1 {
  
  event _reach_e1(T5 _a);

  constructor () payable {
    }

  function m1(address payable acct, uint256 amt) external payable {
    T5 memory a;
    a.time = 0;
    a.msg.v62 = acct;
    a.msg.v63 = amt + 1;
    emit _reach_e1(a);
  }
}
