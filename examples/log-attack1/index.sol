pragma abicoder v2;
pragma solidity ^0.8.0;

struct T2 {
  address payable v58;
  uint256 v59;
  }
struct T3 {
  bool svs;
  T2 msg;
  }
contract LogAttack1 {
  
  event e1(T3 _a);

  constructor () payable {
    }

  function m2(address payable acct, uint256 amt) external payable {
    T3 memory a;
    a.svs = false;
    a.msg.v58 = acct;
    a.msg.v59 = amt + 1;
     emit e1(a);
  }
}