#!/bin/sh
TARGET="$1"

{
  sed '1,4d' "${ERC20}/IERC20.sol";
  sed '1,6d' "${ERC20}/extensions/IERC20Metadata.sol";
  sed '1,4d' "${ERC20}/../../utils/Context.sol";
  sed '1,8d; 158,196d; 215d; 237d; 245,267d; 288,303d' "${ERC20}/ERC20.sol"
} > "$TARGET"
