// log-attack1
pragma abicoder v2;
pragma solidity ^0.8.0;

struct T2 {
  uint256 elem0;
  address payable elem1;
  uint256 elem2;
  }

contract LogAttack1 {
  constructor () payable {
  }

  event _reach_e1(address _who, T2 _a);
  function m1(address payable acct, uint256 amt) external payable {
    T2 memory a;
    a.elem0 = 0;
    a.elem1 = acct;
    a.elem2 = amt + 1;
    emit _reach_e1(msg.sender, a);
  }
}
