// Automatically generated with Reach 0.1.0

pragma solidity ^0.5.11;

pragma solidity ^0.5.11;

contract Stdlib {

  function INT_TO_BYTES (uint256 x)
    internal pure returns (bytes memory) {
    return abi.encodePacked(x); }

  function BCAT (bytes memory l, bytes memory r)
    internal pure returns (bytes memory) {
    return abi.encodePacked(uint16(l.length), l, r); }

  function BCAT_LEFT_LENGTH (bytes memory c)
    internal pure returns (uint16) {
      require(c.length >= 2);
      uint16 len = uint16(uint8(c[0]))*256+uint16(uint8(c[1])); // TODO: improve using some library on the net
      require(c.length >= 2 + uint(len));
      return len; }

  // TODO: optimize using tricks from the Internet? But not before there are tests somehow.
  function BYTES_SLICE (bytes memory _in, uint16 _start, uint16 _len)
    internal pure returns (bytes memory) {
      require(_in.length >= uint(_start) + uint(_len));
      bytes memory out = new bytes(_len);
      uint16 end = _start + _len;
      for (uint16 i = 0; i < end; i++) {
              out[i] = _in[i]; }
      return out; }

  function BCAT_LEFT (bytes memory c)
    internal pure returns (bytes memory) {
      return BYTES_SLICE(c, 2, BCAT_LEFT_LENGTH(c)); }

  function BCAT_RIGHT (bytes memory c)
    internal pure returns (bytes memory) {
      uint16 start = 2 + BCAT_LEFT_LENGTH(c);
      uint16 len = uint16(c.length) - start;
      return BYTES_SLICE(c, start, len); } }


contract ReachContract is Stdlib {
  uint256 current_state;
  
  constructor() public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), uint256(block.number)))); }
  
  event e1(uint256 _bal, uint256 v6, uint256 v7, uint256 v8, uint256 v10);
  struct _F1 {
    address payable v0; }
  function m1(uint256 _last, uint256 v6, uint256 v7, uint256 v8, uint256 v10) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    _F1 memory _f;
    _f.v0 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v6));
    require((v7 < v8));
    emit e1(address(this).balance, v6, v7, v8, v10);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), _f.v0, v6, v7, v8, v10))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(uint256(block.number) >= _last + uint256(10));
    emit e2(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e3(uint256 _bal);
  struct _F3 {
    address payable v5; }
  function m3(uint256 _last, address payable v0, uint256 v6, uint256 v7, uint256 v8, uint256 v10) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v6, v7, v8, v10))));
    _F3 memory _f;
    _f.v5 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v7));
    v0.transfer(v7);
    emit e3(address(this).balance);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v0, _f.v5, v6, v8, v10))); }
  
  event e4(uint256 _bal);
  
  function m4(uint256 _last, address payable v0, uint256 v6, uint256 v7, uint256 v8, uint256 v10) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v6, v7, v8, v10))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10));
    v0.transfer(v6);
    emit e4(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e5(uint256 _bal);
  
  function m5(uint256 _last, address payable v0, address payable v5, uint256 v6, uint256 v8, uint256 v10) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v5, v6, v8, v10))));
    
    require(msg.sender == v0);
    require(uint256(block.number) < _last + v10);
    require(((msg.value) == v8));
    v5.transfer(v8);
    v0.transfer(v6);
    emit e5(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v0, address payable v5, uint256 v6, uint256 v8, uint256 v10) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v5, v6, v8, v10))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + v10);
    v5.transfer(v6);
    emit e6(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }