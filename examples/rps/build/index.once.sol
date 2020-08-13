// Automatically generated with Reach 0.1.0

pragma solidity ^0.5.11;

contract Stdlib { }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() public payable {
    
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), uint256(block.number)))); }
  
  
  
  
  
  
  struct T0 {
    uint256 elem0;
    uint256 elem1; }
  
  event e1(uint256 _bal, uint256 v4, uint256 v5);
  
  function m1(uint256 _last, uint256 v4, uint256 v5) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(true && true);
    require(((v4 + v5) == (msg.value)));
    emit e1(address(this).balance, v4, v5);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), msg.sender, v4, v5))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last, address payable v6, uint256 v4, uint256 v5) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v6, v4, v5))));
    
    
    require(true && uint256(block.number) < _last + uint256(10));
    require((v4 == (msg.value)));
    emit e2(address(this).balance);
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), uint256(block.number), v6, v4, v5, msg.sender))); }
  
  event e3(uint256 _bal, uint256 v55);
  
  function m3(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v14, uint256 v55) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), _last, v6, v4, v5, v14))));
    
    require(msg.sender == v6);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    emit e3(address(this).balance, v55);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v6, v4, v5, v14, v55))); }
  
  event e4(uint256 _bal, uint256 v92);
  
  function m4(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v14, uint256 v55, uint256 v92) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v6, v4, v5, v14, v55))));
    
    require(msg.sender == v14);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v92) ? (v92 < uint256(3)) : false));
    emit e4(address(this).balance, v92);
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), uint256(block.number), v6, v4, v5, v14, v55, v92))); }
  
  event e5(uint256 _bal, uint256 v126, uint256 v127);
  struct _F5 {
    uint256 v155;
    T0 v228;
    bool v160;
    bool v165; }
  function m5(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v14, uint256 v55, uint256 v92, uint256 v126, uint256 v127) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v6, v4, v5, v14, v55, v92))));
    _F5 memory _f;
    require(msg.sender == v6);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v55 == (uint256(keccak256(abi.encodePacked(v126, v127))))));
    require(((uint256(0) <= v127) ? (v127 < uint256(3)) : false));
    _f.v160 = (uint256(0) <= v127) ? (v127 < uint256(3)) : false;
    _f.v165 = (uint256(0) <= v92) ? (v92 < uint256(3)) : false;
    if ((_f.v160 ? _f.v165 : false)) {
      _f.v155 = ((v127 + (uint256(4) - v92)) % uint256(3));
       }
    else {
      if (_f.v160) {
        _f.v155 = uint256(2);
         }
      else {
        if (_f.v165) {
          _f.v155 = uint256(0);
           }
        else {
          _f.v155 = uint256(1);
           }
         }
       }
    if ((_f.v155 == uint256(2))) {
      _f.v228 = T0((uint256(2) * v4), uint256(0));
       }
    else {
      if ((_f.v155 == uint256(0))) {
        _f.v228 = T0(uint256(0), (uint256(2) * v4));
         }
      else {
        _f.v228 = T0(v4, v4);
         }
       }
    v6.transfer((v5 + (_f.v228.elem0)));
    v14.transfer((_f.v228.elem1));
    emit e5(address(this).balance, v126, v127);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v14, uint256 v55, uint256 v92) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v6, v4, v5, v14, v55, v92))));
    
    require(msg.sender == v14);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v14.transfer((address(this).balance));
    emit e6(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e7(uint256 _bal);
  
  function m7(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v14, uint256 v55) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v6, v4, v5, v14, v55))));
    
    require(msg.sender == v6);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v6.transfer((address(this).balance));
    emit e7(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v6, uint256 v4, uint256 v5, address payable v14) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), _last, v6, v4, v5, v14))));
    
    require(msg.sender == v14);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v14.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v6, uint256 v4, uint256 v5) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v6, v4, v5))));
    
    require(msg.sender == v6);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v6.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }