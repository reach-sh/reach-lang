pragma abicoder v2;

pragma solidity ^0.8.0;

struct T2 {
  address payable v74;
  address payable v75;
  }
struct T5 {
  uint256 v90;
  }
struct T6 {
  T2 svs;
  T5 msg;
  }

contract LogAttack2  {

  uint256 date;
  constructor () payable {
  }

  event e2(T6 _a);
  function m1(address payable, uint256 amt) external payable {
    T6 memory a;   
    a.svs.v74 = payable(msg.sender);
    a.svs.v75 = payable(address(this));
    a.msg.v90 = date;
    emit e2(a);
  }
  
  function m2(uint256 date) external payable {
  }
}