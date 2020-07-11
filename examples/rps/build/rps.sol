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
  
  event e1(uint256 _bal, uint256 v4, uint256 v6);
  struct _F1 {
    address payable v0; }
  function m1(uint256 _last, uint256 v4, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    _F1 memory _f;
    _f.v0 = msg.sender;
    require(true && true);
    require(((msg.value) == (v4 + v6)));
    emit e1(address(this).balance, v4, v6);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), _f.v0, v4, v6))); }
  
  event e2(uint256 _bal);
  struct _F2 {
    address payable v1; }
  function m2(uint256 _last, address payable v0, uint256 v4, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v4, v6))));
    _F2 memory _f;
    _f.v1 = msg.sender;
    require(true && uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v4));
    emit e2(address(this).balance);
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), uint256(block.number), v0, _f.v1, v4, v6))); }
  
  event e3(uint256 _bal, uint256 v28);
  
  function m3(uint256 _last, address payable v0, address payable v1, uint256 v4, uint256 v6, uint256 v28) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), _last, v0, v1, v4, v6))));
    
    require(msg.sender == v0);
    require(true && uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    emit e3(address(this).balance, v28);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v0, v1, v4, v6, v28))); }
  
  event e4(uint256 _bal, uint256 v43);
  
  function m4(uint256 _last, address payable v0, address payable v1, uint256 v4, uint256 v6, uint256 v28, uint256 v43) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v1, v4, v6, v28))));
    
    require(msg.sender == v1);
    require(true && uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require(((uint256(0) <= v43) ? (v43 < uint256(3)) : false));
    emit e4(address(this).balance, v43);
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), uint256(block.number), v0, v1, v4, v6, v28, v43))); }
  
  event e5(uint256 _bal, uint256 v50, uint256 v51);
  struct _F5 {
    bool v69;
    bool v72;
    uint256 v79;
    bool v95;
    bool v102;
    bool v109;
    bool v111; }
  function m5(uint256 _last, address payable v0, address payable v1, uint256 v4, uint256 v6, uint256 v28, uint256 v43, uint256 v50, uint256 v51) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v0, v1, v4, v6, v28, v43))));
    _F5 memory _f;
    require(msg.sender == v0);
    require(true && uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require((v28 == (uint256(keccak256(abi.encodePacked(v50, v51))))));
    require(((uint256(0) <= v51) ? (v51 < uint256(3)) : false));
    _f.v69 = (uint256(0) <= v51) ? (v51 < uint256(3)) : false;
    _f.v72 = (uint256(0) <= v43) ? (v43 < uint256(3)) : false;
    _f.v79 = (_f.v69 ? _f.v72 : false) ? ((v51 + (uint256(4) - v43)) % uint256(3)) : (_f.v69 ? uint256(2) : (_f.v72 ? uint256(0) : uint256(1)));
    _f.v95 = _f.v79 == uint256(2);
    _f.v102 = _f.v79 == uint256(0);
    _f.v109 = _f.v79 == uint256(2);
    _f.v111 = _f.v79 == uint256(0);
    v0.transfer((v6 + (_f.v109 ? (uint256(2) * v4) : (_f.v111 ? uint256(0) : v4))));
    v1.transfer((_f.v109 ? uint256(0) : (_f.v111 ? (uint256(2) * v4) : v4)));
    emit e5(address(this).balance, v50, v51);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v0, address payable v1, uint256 v4, uint256 v6, uint256 v28, uint256 v43) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), _last, v0, v1, v4, v6, v28, v43))));
    
    require(msg.sender == v1);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require(((msg.value) == uint256(0)));
    v1.transfer((address(this).balance));
    emit e6(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e7(uint256 _bal);
  
  function m7(uint256 _last, address payable v0, address payable v1, uint256 v4, uint256 v6, uint256 v28) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v1, v4, v6, v28))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require(((msg.value) == uint256(0)));
    v0.transfer((address(this).balance));
    emit e7(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v0, address payable v1, uint256 v4, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), _last, v0, v1, v4, v6))));
    
    require(msg.sender == v1);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require(((msg.value) == uint256(0)));
    v1.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal);
  
  function m9(uint256 _last, address payable v0, uint256 v4, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v4, v6))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10) && true);
    require(((msg.value) == uint256(0)));
    v0.transfer((address(this).balance));
    emit e9(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }