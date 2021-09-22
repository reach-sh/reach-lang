pragma abicoder v2;

pragma solidity ^0.8.0;

struct T1 {
  address payable v59;
  address payable v60;
  uint256 v61;
  }
struct T5 {
  T1 svs;
  bool msg;
  }

contract WeirdContract {
  uint256 _x;
  constructor() payable {
    _x = 0;
  }

  event e1(T5 _a);
  function f(uint256 x) external payable {
    T5 memory a;
    a.svs.v59 = payable(msg.sender);
    a.svs.v60 = payable(msg.sender);
    a.svs.v61 = x + 1;
    a.msg = true;
    emit e1(a);
    _x = x;
  }
}
