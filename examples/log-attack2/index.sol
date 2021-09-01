pragma abicoder v2;

pragma solidity ^0.8.0;

struct T2 {
  address payable v56;
  }
struct T5 {
  uint256 v71;
  }
struct T6 {
  T2 svs;
  T5 msg;
  }

contract LogAttack2  {
  constructor () payable {
  }

  event e2(T6 _a);
  function m2() external payable {
    T6 memory a;
    a.svs.v56 = payable(msg.sender);
    a.msg.v71 = 1337;
    emit e2(a);
  }
}
