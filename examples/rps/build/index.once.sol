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
  
  event e1(uint256 _bal, uint256 v5, uint256 v6);
  
  function m1(uint256 _last, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(true && true);
    require(((v5 + v6) == (msg.value)));
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), msg.sender, v5, v6)));
    emit e1(address(this).balance, v5, v6); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v7, v5, v6))));
    
    
    require(true && uint256(block.number) < _last + uint256(10));
    require((v5 == (msg.value)));
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), uint256(block.number), v7, v5, v6, msg.sender)));
    emit e2(address(this).balance); }
  
  event e3(uint256 _bal, uint256 v48);
  
  function m3(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v48) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), _last, v7, v5, v6, v15))));
    
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10));
    require((uint256(0) == (msg.value)));
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v7, v5, v6, v15, v48)));
    emit e3(address(this).balance, v48); }
  
  event e4(uint256 _bal, uint256 v77);
  
  function m4(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v48, uint256 v77) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v7, v5, v6, v15, v48))));
    
    require(msg.sender == v15);
    require(true && uint256(block.number) < _last + uint256(10) + uint256(10) + uint256(10));
    require((uint256(0) == (msg.value)));
    require(((uint256(0) <= v77) ? (v77 < uint256(3)) : false));
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), uint256(block.number), v7, v5, v6, v15, v48, v77)));
    emit e4(address(this).balance, v77); }
  
  event e5(uint256 _bal, uint256 v103, uint256 v104);
  struct _F5 {
    uint256 v124;
    T0 v197;
    bool v129;
    bool v134; }
  function m5(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v48, uint256 v77, uint256 v103, uint256 v104) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v7, v5, v6, v15, v48, v77))));
    _F5 memory _f;
    require(msg.sender == v7);
    require(true && uint256(block.number) < _last + uint256(10) + uint256(10) + uint256(10) + uint256(10));
    require((uint256(0) == (msg.value)));
    require((v48 == (uint256(keccak256(abi.encodePacked(v103, v104))))));
    require(((uint256(0) <= v104) ? (v104 < uint256(3)) : false));
    _f.v129 = (uint256(0) <= v104) ? (v104 < uint256(3)) : false;
    _f.v134 = (uint256(0) <= v77) ? (v77 < uint256(3)) : false;
    if ((_f.v129 ? _f.v134 : false)) {
      _f.v124 = ((v104 + (uint256(4) - v77)) % uint256(3));
       }
    else {
      if (_f.v129) {
        _f.v124 = uint256(2);
         }
      else {
        if (_f.v134) {
          _f.v124 = uint256(0);
           }
        else {
          _f.v124 = uint256(1);
           }
         }
       }
    if ((_f.v124 == uint256(2))) {
      _f.v197 = T0((uint256(2) * v5), uint256(0));
       }
    else {
      if ((_f.v124 == uint256(0))) {
        _f.v197 = T0(uint256(0), (uint256(2) * v5));
         }
      else {
        _f.v197 = T0(v5, v5);
         }
       }
    v7.transfer((v6 + (_f.v197.elem0)));
    v15.transfer((_f.v197.elem1));
    emit e5(address(this).balance, v103, v104);
    current_state = 0x0;
    selfdestruct(msg.sender);
     }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v48, uint256 v77) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v7, v5, v6, v15, v48, v77))));
    
    require(msg.sender == v15);
    require(uint256(block.number) >= _last + uint256(10) && uint256(block.number) < _last + uint256(10) + uint256(10) + uint256(10));
    require((uint256(0) == (msg.value)));
    v15.transfer((address(this).balance));
    current_state = 0x0;
    selfdestruct(msg.sender);
    emit e6(address(this).balance); }
  
  event e7(uint256 _bal);
  
  function m7(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15, uint256 v48) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v7, v5, v6, v15, v48))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && uint256(block.number) < _last + uint256(10) + uint256(10));
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    current_state = 0x0;
    selfdestruct(msg.sender);
    emit e7(address(this).balance); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v7, uint256 v5, uint256 v6, address payable v15) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), _last, v7, v5, v6, v15))));
    
    require(msg.sender == v15);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v15.transfer((address(this).balance));

    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v7, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v7, v5, v6))));
    
    require(msg.sender == v7);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require((uint256(0) == (msg.value)));
    v7.transfer((address(this).balance));
    current_state = 0x0;
    selfdestruct(msg.sender);
    emit e9(address(this).balance); } }
