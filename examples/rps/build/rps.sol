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
  
  event e1(uint256 _bal, uint256 v5, uint256 v6);
  struct _F1 {
    address payable v0; }
  function m1(uint256 _last, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    _F1 memory _f;
    _f.v0 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == (v5 + v6)));
    emit e1(address(this).balance, v5, v6);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), uint256(block.number), _f.v0, v5, v6))); }
  
  event e2(uint256 _bal);
  
  function m2(uint256 _last) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), _last))));
    
    
    require(uint256(block.number) >= _last + uint256(10));
    emit e2(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e3(uint256 _bal);
  struct _F3 {
    address payable v3; }
  function m3(uint256 _last, address payable v0, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v5, v6))));
    _F3 memory _f;
    _f.v3 = msg.sender;
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == v5));
    emit e3(address(this).balance);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), uint256(block.number), v0, _f.v3, v5, v6))); }
  
  event e4(uint256 _bal);
  
  function m4(uint256 _last, address payable v0, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), _last, v0, v5, v6))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10));
    v0.transfer((address(this).balance));
    emit e4(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e5(uint256 _bal, uint256 v31);
  
  function m5(uint256 _last, address payable v0, address payable v3, uint256 v5, uint256 v6, uint256 v31) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v3, v5, v6))));
    
    require(msg.sender == v0);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    emit e5(address(this).balance, v31);
    current_state = uint256(keccak256(abi.encodePacked(uint256(5), uint256(block.number), v0, v3, v5, v6, v31))); }
  
  event e6(uint256 _bal);
  
  function m6(uint256 _last, address payable v0, address payable v3, uint256 v5, uint256 v6) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), _last, v0, v3, v5, v6))));
    
    require(msg.sender == v3);
    require(uint256(block.number) >= _last + uint256(10));
    v3.transfer((address(this).balance));
    emit e6(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e7(uint256 _bal, uint256 v46);
  
  function m7(uint256 _last, address payable v0, address payable v3, uint256 v5, uint256 v6, uint256 v31, uint256 v46) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v0, v3, v5, v6, v31))));
    
    require(msg.sender == v3);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require(((uint256(0) <= v46) ? (v46 < uint256(3)) : false));
    emit e7(address(this).balance, v46);
    current_state = uint256(keccak256(abi.encodePacked(uint256(7), uint256(block.number), v0, v3, v5, v6, v31, v46))); }
  
  event e8(uint256 _bal);
  
  function m8(uint256 _last, address payable v0, address payable v3, uint256 v5, uint256 v6, uint256 v31) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(5), _last, v0, v3, v5, v6, v31))));
    
    require(msg.sender == v0);
    require(uint256(block.number) >= _last + uint256(10));
    v0.transfer((address(this).balance));
    emit e8(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e9(uint256 _bal, uint256 v53, uint256 v54);
  struct _F9 {
    bool v75;
    bool v78;
    uint256 v85;
    bool v101;
    bool v108;
    bool v115;
    bool v117; }
  function m9(uint256 _last, address payable v0, address payable v3, uint256 v5, uint256 v6, uint256 v31, uint256 v46, uint256 v53, uint256 v54) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(7), _last, v0, v3, v5, v6, v31, v46))));
    _F9 memory _f;
    require(msg.sender == v0);
    require(uint256(block.number) < _last + uint256(10));
    require(((msg.value) == uint256(0)));
    require((v31 == (uint256(keccak256(abi.encodePacked((BCAT((abi.encodePacked(v53)), (abi.encodePacked(v54))))))))));
    require(((uint256(0) <= v54) ? (v54 < uint256(3)) : false));
    _f.v75 = (uint256(0) <= v54) ? (v54 < uint256(3)) : false;
    _f.v78 = (uint256(0) <= v46) ? (v46 < uint256(3)) : false;
    _f.v85 = (_f.v75 ? _f.v78 : false) ? ((v54 + (uint256(4) - v46)) % uint256(3)) : (_f.v75 ? uint256(2) : (_f.v78 ? uint256(0) : uint256(1)));
    _f.v101 = _f.v85 == uint256(2);
    _f.v108 = _f.v85 == uint256(0);
    _f.v115 = _f.v85 == uint256(2);
    _f.v117 = _f.v85 == uint256(0);
    v0.transfer((v6 + (_f.v115 ? (uint256(2) * v5) : (_f.v117 ? uint256(0) : v5))));
    v3.transfer((_f.v115 ? uint256(0) : (_f.v117 ? (uint256(2) * v5) : v5)));
    emit e9(address(this).balance, v53, v54);
    current_state = 0x0;
    selfdestruct(msg.sender); }
  
  event e10(uint256 _bal);
  
  function m10(uint256 _last, address payable v0, address payable v3, uint256 v5, uint256 v6, uint256 v31, uint256 v46) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(7), _last, v0, v3, v5, v6, v31, v46))));
    
    require(msg.sender == v3);
    require(uint256(block.number) >= _last + uint256(10));
    v3.transfer((address(this).balance));
    emit e10(address(this).balance);
    current_state = 0x0;
    selfdestruct(msg.sender); } }