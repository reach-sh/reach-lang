pragma abicoder v2;
pragma solidity ^0.8.0;

struct T2 {
  address payable v52;
  uint256 v53;
  }
struct T3 {
  bool svs;
  T2 msg;
  }
contract LogAttack1 {
  
  event e1(T3 _a);

  constructor () payable {
    }

  function m1(address payable acct, uint256 amt) external payable {
    T3 memory a;
    a.svs = false;
    a.msg.v52 = acct;
    a.msg.v53 = amt + 1;
     emit e1(a);
  }
}
