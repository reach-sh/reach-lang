// Automatically generated with Reach 0.1.2
pragma experimental ABIEncoderV2;

pragma solidity ^0.7.1;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() payable {
    
    current_state = uint256(keccak256(abi.encode(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  
  
  event e1(uint256 v27);
  struct a1 {
    uint256 _last;
    uint256 v27; }
  struct _F1 {
    uint256 v32; }
  function m1(a1 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(0), _a._last))));
    _F1 memory _f;
    
    require(true && true);
    require((msg.value == _a.v27));
    _f.v32 = (uint256(0)) + msg.value;
    emit e1(_a.v27);
    current_state = uint256(keccak256(abi.encode(uint256(1), uint256(block.number), _f.v32, msg.sender, _a.v27))); }
  
  event e2();
  struct a2 {
    uint256 _last;
    uint256 v32;
    address payable v28;
    uint256 v27; }
  
  function m2(a2 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v32, _a.v28, _a.v27))));
    
    
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((msg.value == _a.v27));
    emit e2();
    l3(a3(_a.v28, _a.v27, msg.sender, (((_a.v32) + msg.value)), uint256(1))); }
  
  struct a3 {
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v58;
    uint256 v59; }
  
  function l3(a3 memory _a)  internal {
    
    
    if ((_a.v59 == uint256(1))) {
      
      current_state = uint256(keccak256(abi.encode(uint256(3), uint256(block.number), _a.v28, _a.v27, _a.v35, _a.v58))); }
    else {
      ((_a.v59 == uint256(2)) ? _a.v28 : _a.v35).transfer((uint256(2) * _a.v27));
      
      current_state = 0x0;
      selfdestruct(msg.sender); } }
  
  event e4(uint256 v76);
  struct a4 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v58;
    uint256 v76; }
  struct _F4 {
    uint256 v97; }
  function m4(a4 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v28, _a.v27, _a.v35, _a.v58))));
    _F4 memory _f;
    require(msg.sender == _a.v28);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((msg.value == uint256(0)));
    _f.v97 = (_a.v58) + msg.value;
    emit e4(_a.v76);
    current_state = uint256(keccak256(abi.encode(uint256(4), uint256(block.number), _f.v97, _a.v28, _a.v27, _a.v35, _a.v76))); }
  
  event e5(uint256 v100);
  struct a5 {
    uint256 _last;
    uint256 v97;
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v76;
    uint256 v100; }
  struct _F5 {
    uint256 v121; }
  function m5(a5 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v97, _a.v28, _a.v27, _a.v35, _a.v76))));
    _F5 memory _f;
    require(msg.sender == _a.v35);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((msg.value == uint256(0)));
    _f.v121 = (_a.v97) + msg.value;
    emit e5(_a.v100);
    current_state = uint256(keccak256(abi.encode(uint256(5), uint256(block.number), _f.v121, _a.v28, _a.v27, _a.v35, _a.v76, _a.v100))); }
  
  event e6(uint256 v123, uint256 v124);
  struct a6 {
    uint256 _last;
    uint256 v121;
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v76;
    uint256 v100;
    uint256 v123;
    uint256 v124; }
  
  function m6(a6 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v121, _a.v28, _a.v27, _a.v35, _a.v76, _a.v100))));
    
    require(msg.sender == _a.v28);
    require(true && uint256(block.number) < _a._last + uint256(10));
    require((msg.value == uint256(0)));
    require((_a.v76 == (uint256(keccak256(abi.encode(_a.v123, _a.v124))))));
    emit e6(_a.v123, _a.v124);
    l3(a3(_a.v28, _a.v27, _a.v35, (((_a.v121) + msg.value)), ((_a.v124 + (uint256(4) - _a.v100)) % uint256(3)))); }
  
  event e7();
  struct a7 {
    uint256 _last;
    uint256 v121;
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v76;
    uint256 v100; }
  
  function m7(a7 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(5), _a._last, _a.v121, _a.v28, _a.v27, _a.v35, _a.v76, _a.v100))));
    
    require(msg.sender == _a.v35);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((msg.value == uint256(0)));
    _a.v35.transfer((((_a.v121) + msg.value)));
    emit e7();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8();
  struct a8 {
    uint256 _last;
    uint256 v97;
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v76; }
  
  function m8(a8 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(4), _a._last, _a.v97, _a.v28, _a.v27, _a.v35, _a.v76))));
    
    require(msg.sender == _a.v28);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((msg.value == uint256(0)));
    _a.v28.transfer((((_a.v97) + msg.value)));
    emit e8();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9();
  struct a9 {
    uint256 _last;
    address payable v28;
    uint256 v27;
    address payable v35;
    uint256 v58; }
  
  function m9(a9 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(3), _a._last, _a.v28, _a.v27, _a.v35, _a.v58))));
    
    require(msg.sender == _a.v35);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((msg.value == uint256(0)));
    _a.v35.transfer((((_a.v58) + msg.value)));
    emit e9();
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10();
  struct a10 {
    uint256 _last;
    uint256 v32;
    address payable v28;
    uint256 v27; }
  
  function m10(a10 calldata _a) external payable {
    require(current_state == uint256(keccak256(abi.encode(uint256(1), _a._last, _a.v32, _a.v28, _a.v27))));
    
    require(msg.sender == _a.v28);
    require(uint256(block.number) >= _a._last + uint256(10) && true);
    require((msg.value == uint256(0)));
    _a.v28.transfer((((_a.v32) + msg.value)));
    emit e10();
    current_state = 0x0;
    selfdestruct(msg.sender); } }