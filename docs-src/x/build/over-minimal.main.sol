// Automatically generated with Reach 0.1.0
pragma experimental ABIEncoderV2;

pragma solidity ^0.7.0;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() payable {
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  
  event e1(uint256 _bal, uint256 v1);
  
  function m1(uint256 _last, uint256 v1) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _last))));
    
    
    require(true && true);
    require((uint256(0) == (msg.value)));
    emit e1(address(this).balance, v1);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), msg.sender, v1))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last, address payable v2, uint256 v1) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _last, v2, v1))));
    
    
    require(true && true);
    require((v1 == (msg.value)));
    emit e2(address(this).balance);
    current_state = uint256(keccak256(abi.encode(uint256(2), uint256(block.number), v2, v1))); }
  
  event e3(uint256 _bal, bytes v11);
  
  function m3(uint256 _last, address payable v2, uint256 v1, bytes calldata v11) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _last, v2, v1))));
    
    require(msg.sender == v2);
    require(true && true);
    require((uint256(0) == (msg.value)));
    v2.transfer(v1);
    emit e3(address(this).balance, v11);
    current_state = 0x0;
    selfdestruct(msg.sender); } }