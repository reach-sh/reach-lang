// Automatically generated with Reach 0.1.2
pragma experimental ABIEncoderV2;

pragma solidity ^0.7.1;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() payable {
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  
  event e1(uint256 v1);
  struct a1 {
    uint256 _last;
    uint256 v1; }
  
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a._last))));
    
    
    require(true && true);
    require((msg.value == uint256(0)));
    emit e1(_a.v1);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), msg.sender, _a.v1))); }
  
  event e2();
  struct a2 {
    uint256 _last;
    address payable v2;
    uint256 v1; }
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v2, _a.v1))));
    
    
    require(true && true);
    require((msg.value == _a.v1));
    emit e2();
    current_state = uint256(keccak256(abi.encode(uint256(2), uint256(block.number), _a.v2, _a.v1))); }
  
  event e3(bytes v13);
  struct a3 {
    uint256 _last;
    address payable v2;
    uint256 v1;
    bytes v13; }
  
  function m3(a3 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(2), _a._last, _a.v2, _a.v1))));
    
    require(msg.sender == _a.v2);
    require(true && true);
    require((msg.value == uint256(0)));
    _a.v2.transfer(_a.v1);
    emit e3(_a.v13);
    current_state = 0x0;
    selfdestruct(msg.sender); } }