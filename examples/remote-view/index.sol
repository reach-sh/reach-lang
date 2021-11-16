pragma abicoder v2;

pragma solidity ^0.8.0;

interface ReachContract {
  function currentInt()  external view returns (uint256);
}

contract MyContract {

  constructor() {
  }

  function callView(address payable addr) external returns (uint256 curInt) {
    (bool ok, bytes memory ret) = addr.call(abi.encodeWithSelector(ReachContract.currentInt.selector));
    curInt = abi.decode(ret, (uint256));
  }

}
