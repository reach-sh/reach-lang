pragma abicoder v2;

pragma solidity ^0.8.0;

interface ReachContract {
  function incr(uint256)  external returns (uint256);
}

contract MyContract {

  constructor() {
  }

  function callApi(address payable addr, uint256 i) external payable returns (uint256 curCounter) {
    (bool ok, bytes memory ret) = addr.call(abi.encodeWithSelector(ReachContract.incr.selector, i));
    curCounter = abi.decode(ret, (uint256));
  }

}
