// SPDX-License-Identifier: MIT
pragma abicoder v2;


pragma solidity ^0.8.0;

contract LogAttack2  {
    
     event e0();
     event e2(address , uint256 );
     event e3(address , uint256 date, uint256 );

    constructor () payable {
        emit e0();
    }
  function m2(address payable , uint256 ) external payable returns (bool) {
     emit e2(msg.sender, msg.value);
     return true;
  }
  function m3(uint256 date) external payable {
      emit e3(msg.sender, date, msg.value);
  }
}