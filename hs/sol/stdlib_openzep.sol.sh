#!/bin/sh
# Delete pragmas and imports
sed '1,4d' "${ERC20}/IERC20.sol";
sed '1,6d' "${ERC20}/extensions/IERC20Metadata.sol";
sed '1,4d' "${ERC20}/../../utils/Context.sol";
# Delete pragma
# Delete import
# RM Delete (in|de)creaseAllowance
# RM Delete _beforeTokenTransfer (and calls)
sed '1,8d' "${ERC20}/ERC20.sol"
