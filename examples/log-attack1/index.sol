pragma abicoder v2;
pragma solidity ^0.8.0;

struct T0 {
  address payable v49;
  }
struct T4 {
  address payable v59;
  uint256 v60;
  }
struct T5 {
  T0 svs;
  T4 msg;
  }
contract LogAttack1 {
  
  event e1(T5 _a);

  constructor () payable {
    }

  function m1(address payable acct, uint256 amt) external payable {
    T5 memory a;
    a.svs.v49 = acct;
    a.msg.v59 = acct;
    a.msg.v60 = amt + 1;
    emit e1(a);
  }
}
