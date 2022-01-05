pragma abicoder v2;

pragma solidity ^0.8.0;

interface ReachContract {
  function currentInt() external view returns (uint256);
}

contract MyContract {
  event CallView(uint256);

  constructor() {
  }

  function callView(address payable addr) external {
    (bool ok, bytes memory ret) = addr.call(abi.encodeWithSelector(ReachContract.currentInt.selector));
    require(ok);
    emit CallView(abi.decode(ret, (uint256)));
  }

}
