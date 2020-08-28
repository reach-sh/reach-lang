// Automatically generated with Reach 0.1.0
pragma experimental ABIEncoderV2;

pragma solidity ^0.7.0;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() payable {
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  
  event e1(uint256 _bal, uint256 v27);
  struct a1 {
    uint256 _last;
    uint256 v27; }
  
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a._last))));
    
    
    require(true && true);
    require((_a.v27 == (msg.value)));
    emit e1(address(this).balance, _a.v27);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), msg.sender, _a.v27))); }
  
  event e2(uint256 _bal);
  struct a2 {
    uint256 _last;
    address payable v28;
    uint256 v27; }
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v28, _a.v27))));
    
    
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((_a.v27 == (msg.value)));
    emit e2(address(this).balance);
    l3(a3(_a.v28, _a.v27, msg.sender, uint256(1))); }
  
  struct a3 {
    address payable v28;
    uint256 v27;
    address payable v34;
    uint256 v48; }
  
  function l3(a3 memory _a) internal {
    
    
    if ((_a.v48 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(3), uint256(block.number), _a.v28, _a.v27, _a.v34))); }
    else {
      ((_a.v48 == uint256(2)) ? _a.v28 : _a.v34).transfer((uint256(2) * _a.v27));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e4(uint256 _bal, uint256 v65);
  struct a4 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v34;
    uint256 v65; }
  
  function m4(a4 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v28, _a.v27, _a.v34))));
    
    require(msg.sender == _a.v28);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e4(address(this).balance, _a.v65);
    current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), _a.v28, _a.v27, _a.v34, _a.v65))); }
  
  event e5(uint256 _bal, uint256 v81);
  struct a5 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v34;
    uint256 v65;
    uint256 v81; }
  
  function m5(a5 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v28, _a.v27, _a.v34, _a.v65))));
    
    require(msg.sender == _a.v34);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e5(address(this).balance, _a.v81);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), _a.v28, _a.v27, _a.v34, _a.v65, _a.v81))); }
  
  event e6(uint256 _bal, uint256 v96, uint256 v97);
  struct a6 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v34;
    uint256 v65;
    uint256 v81;
    uint256 v96;
    uint256 v97; }
  
  function m6(a6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v28, _a.v27, _a.v34, _a.v65, _a.v81))));
    
    require(msg.sender == _a.v28);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((_a.v65 == (uint256(keccak256(abi.encode(_a.v96, _a.v97))))));
    emit e6(address(this).balance, _a.v96, _a.v97);
    l3(a3(_a.v28, _a.v27, _a.v34, ((_a.v97 + (uint256(4) - _a.v81)) % uint256(3)))); }
  
  event e7(uint256 _bal);
  struct a7 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v34;
    uint256 v65;
    uint256 v81; }
  
  function m7(a7 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v28, _a.v27, _a.v34, _a.v65, _a.v81))));
    
    require(msg.sender == _a.v34);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v34.transfer((address(this).balance));
    emit e7(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8(uint256 _bal);
  struct a8 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v34;
    uint256 v65; }
  
  function m8(a8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v28, _a.v27, _a.v34, _a.v65))));
    
    require(msg.sender == _a.v28);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v28.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  struct a9 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v34; }
  
  function m9(a9 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v28, _a.v27, _a.v34))));
    
    require(msg.sender == _a.v34);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v34.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  struct a10 {
    uint256 _last;
    address payable v28;
    uint256 v27; }
  
  function m10(a10 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v28, _a.v27))));
    
    require(msg.sender == _a.v28);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    _a.v28.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }