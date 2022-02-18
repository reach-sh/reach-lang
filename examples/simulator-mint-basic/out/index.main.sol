// Automatically generated with Reach 0.1.8 (2460e416*)
pragma abicoder v2;

pragma solidity ^0.8.9;
/**
 * @dev Interface of the ERC20 standard as defined in the EIP.
 */
interface IERC20 {
    /**
     * @dev Returns the amount of tokens in existence.
     */
    function totalSupply() external view returns (uint256);

    /**
     * @dev Returns the amount of tokens owned by `account`.
     */
    function balanceOf(address account) external view returns (uint256);

    /**
     * @dev Moves `amount` tokens from the caller's account to `recipient`.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    function transfer(address recipient, uint256 amount) external returns (bool);

    /**
     * @dev Returns the remaining number of tokens that `spender` will be
     * allowed to spend on behalf of `owner` through {transferFrom}. This is
     * zero by default.
     *
     * This value changes when {approve} or {transferFrom} are called.
     */
    function allowance(address owner, address spender) external view returns (uint256);

    /**
     * @dev Sets `amount` as the allowance of `spender` over the caller's tokens.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * IMPORTANT: Beware that changing an allowance with this method brings the risk
     * that someone may use both the old and the new allowance by unfortunate
     * transaction ordering. One possible solution to mitigate this race
     * condition is to first reduce the spender's allowance to 0 and set the
     * desired value afterwards:
     * https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
     *
     * Emits an {Approval} event.
     */
    function approve(address spender, uint256 amount) external returns (bool);

    /**
     * @dev Moves `amount` tokens from `sender` to `recipient` using the
     * allowance mechanism. `amount` is then deducted from the caller's
     * allowance.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    function transferFrom(address sender, address recipient, uint256 amount) external returns (bool);

    /**
     * @dev Emitted when `value` tokens are moved from one account (`from`) to
     * another (`to`).
     *
     * Note that `value` may be zero.
     */
    event Transfer(address indexed from, address indexed to, uint256 value);

    /**
     * @dev Emitted when the allowance of a `spender` for an `owner` is set by
     * a call to {approve}. `value` is the new allowance.
     */
    event Approval(address indexed owner, address indexed spender, uint256 value);
}
/**
 * @dev Interface for the optional metadata functions from the ERC20 standard.
 *
 * _Available since v4.1._
 */
interface IERC20Metadata is IERC20 {
    /**
     * @dev Returns the name of the token.
     */
    function name() external view returns (string memory);

    /**
     * @dev Returns the symbol of the token.
     */
    function symbol() external view returns (string memory);

    /**
     * @dev Returns the decimals places of the token.
     */
    function decimals() external view returns (uint8);
}
/*
 * @dev Provides information about the current execution context, including the
 * sender of the transaction and its data. While these are generally available
 * via msg.sender and msg.data, they should not be accessed in such a direct
 * manner, since when dealing with meta-transactions the account sending and
 * paying for execution may not be the actual sender (as far as an application
 * is concerned).
 *
 * This contract is only required for intermediate, library-like contracts.
 */
abstract contract Context {
    function _msgSender() internal view virtual returns (address) {
        return msg.sender;
    }

    function _msgData() internal view virtual returns (bytes calldata) {
        this; // silence state mutability warning without generating bytecode - see https://github.com/ethereum/solidity/issues/2691
        return msg.data;
    }
}
/**
 * @dev Implementation of the {IERC20} interface.
 *
 * This implementation is agnostic to the way tokens are created. This means
 * that a supply mechanism has to be added in a derived contract using {_mint}.
 * For a generic mechanism see {ERC20PresetMinterPauser}.
 *
 * TIP: For a detailed writeup see our guide
 * https://forum.zeppelin.solutions/t/how-to-implement-erc20-supply-mechanisms/226[How
 * to implement supply mechanisms].
 *
 * We have followed general OpenZeppelin guidelines: functions revert instead
 * of returning `false` on failure. This behavior is nonetheless conventional
 * and does not conflict with the expectations of ERC20 applications.
 *
 * Additionally, an {Approval} event is emitted on calls to {transferFrom}.
 * This allows applications to reconstruct the allowance for all accounts just
 * by listening to said events. Other implementations of the EIP may not emit
 * these events, as it isn't required by the specification.
 *
 * Finally, the non-standard {decreaseAllowance} and {increaseAllowance}
 * functions have been added to mitigate the well-known issues around setting
 * allowances. See {IERC20-approve}.
 */
contract ERC20 is Context, IERC20, IERC20Metadata {
    mapping (address => uint256) private _balances;

    mapping (address => mapping (address => uint256)) private _allowances;

    uint256 private _totalSupply;

    string private _name;
    string private _symbol;

    /**
     * @dev Sets the values for {name} and {symbol}.
     *
     * The defaut value of {decimals} is 18. To select a different value for
     * {decimals} you should overload it.
     *
     * All two of these values are immutable: they can only be set once during
     * construction.
     */
    constructor (string memory name_, string memory symbol_) {
        _name = name_;
        _symbol = symbol_;
    }

    /**
     * @dev Returns the name of the token.
     */
    function name() public view virtual override returns (string memory) {
        return _name;
    }

    /**
     * @dev Returns the symbol of the token, usually a shorter version of the
     * name.
     */
    function symbol() public view virtual override returns (string memory) {
        return _symbol;
    }

    /**
     * @dev Returns the number of decimals used to get its user representation.
     * For example, if `decimals` equals `2`, a balance of `505` tokens should
     * be displayed to a user as `5,05` (`505 / 10 ** 2`).
     *
     * Tokens usually opt for a value of 18, imitating the relationship between
     * Ether and Wei. This is the value {ERC20} uses, unless this function is
     * overridden;
     *
     * NOTE: This information is only used for _display_ purposes: it in
     * no way affects any of the arithmetic of the contract, including
     * {IERC20-balanceOf} and {IERC20-transfer}.
     */
    function decimals() public view virtual override returns (uint8) {
        return 18;
    }

    /**
     * @dev See {IERC20-totalSupply}.
     */
    function totalSupply() public view virtual override returns (uint256) {
        return _totalSupply;
    }

    /**
     * @dev See {IERC20-balanceOf}.
     */
    function balanceOf(address account) public view virtual override returns (uint256) {
        return _balances[account];
    }

    /**
     * @dev See {IERC20-transfer}.
     *
     * Requirements:
     *
     * - `recipient` cannot be the zero address.
     * - the caller must have a balance of at least `amount`.
     */
    function transfer(address recipient, uint256 amount) public virtual override returns (bool) {
        _transfer(_msgSender(), recipient, amount);
        return true;
    }

    /**
     * @dev See {IERC20-allowance}.
     */
    function allowance(address owner, address spender) public view virtual override returns (uint256) {
        return _allowances[owner][spender];
    }

    /**
     * @dev See {IERC20-approve}.
     *
     * Requirements:
     *
     * - `spender` cannot be the zero address.
     */
    function approve(address spender, uint256 amount) public virtual override returns (bool) {
        _approve(_msgSender(), spender, amount);
        return true;
    }

    /**
     * @dev See {IERC20-transferFrom}.
     *
     * Emits an {Approval} event indicating the updated allowance. This is not
     * required by the EIP. See the note at the beginning of {ERC20}.
     *
     * Requirements:
     *
     * - `sender` and `recipient` cannot be the zero address.
     * - `sender` must have a balance of at least `amount`.
     * - the caller must have allowance for ``sender``'s tokens of at least
     * `amount`.
     */
    function transferFrom(address sender, address recipient, uint256 amount) public virtual override returns (bool) {
        _transfer(sender, recipient, amount);

        uint256 currentAllowance = _allowances[sender][_msgSender()];
        require(currentAllowance >= amount, "ERC20: transfer amount exceeds allowance");
        _approve(sender, _msgSender(), currentAllowance - amount);

        return true;
    }

    /**
     * @dev Moves tokens `amount` from `sender` to `recipient`.
     *
     * This is internal function is equivalent to {transfer}, and can be used to
     * e.g. implement automatic token fees, slashing mechanisms, etc.
     *
     * Emits a {Transfer} event.
     *
     * Requirements:
     *
     * - `sender` cannot be the zero address.
     * - `recipient` cannot be the zero address.
     * - `sender` must have a balance of at least `amount`.
     */
    function _transfer(address sender, address recipient, uint256 amount) internal virtual {
        require(sender != address(0), "ERC20: transfer from the zero address");
        require(recipient != address(0), "ERC20: transfer to the zero address");


        uint256 senderBalance = _balances[sender];
        require(senderBalance >= amount, "ERC20: transfer amount exceeds balance");
        _balances[sender] = senderBalance - amount;
        _balances[recipient] += amount;

        emit Transfer(sender, recipient, amount);
    }

    /** @dev Creates `amount` tokens and assigns them to `account`, increasing
     * the total supply.
     *
     * Emits a {Transfer} event with `from` set to the zero address.
     *
     * Requirements:
     *
     * - `to` cannot be the zero address.
     */
    function _mint(address account, uint256 amount) internal virtual {
        require(account != address(0), "ERC20: mint to the zero address");


        _totalSupply += amount;
        _balances[account] += amount;
        emit Transfer(address(0), account, amount);
    }

    /**
     * @dev Destroys `amount` tokens from `account`, reducing the
     * total supply.
     *
     * Emits a {Transfer} event with `to` set to the zero address.
     *
     * Requirements:
     *
     * - `account` cannot be the zero address.
     * - `account` must have at least `amount` tokens.
     */
    function _burn(address account, uint256 amount) internal virtual {
        require(account != address(0), "ERC20: burn from the zero address");


        uint256 accountBalance = _balances[account];
        require(accountBalance >= amount, "ERC20: burn amount exceeds balance");
        _balances[account] = accountBalance - amount;
        _totalSupply -= amount;

        emit Transfer(account, address(0), amount);
    }

    /**
     * @dev Sets `amount` as the allowance of `spender` over the `owner` s tokens.
     *
     * This internal function is equivalent to `approve`, and can be used to
     * e.g. set automatic allowances for certain subsystems, etc.
     *
     * Emits an {Approval} event.
     *
     * Requirements:
     *
     * - `owner` cannot be the zero address.
     * - `spender` cannot be the zero address.
     */
    function _approve(address owner, address spender, uint256 amount) internal virtual {
        require(owner != address(0), "ERC20: approve from the zero address");
        require(spender != address(0), "ERC20: approve to the zero address");

        _allowances[owner][spender] = amount;
        emit Approval(owner, spender, amount);
    }
}
/*
  ReachToken essentially emulates Algorand Standard Assets on Ethereum, but doesn't include things like clawback or a separation of management and creator.
 */
contract ReachToken is ERC20 {
  address private _creator;
  string private _url;
  string private _metadata;
  uint8 private _decimals;

  constructor (
    string memory name_,
    string memory symbol_,
    string memory url_,
    string memory metadata_,
    uint256 supply_,
    uint256 decimals_
  ) ERC20(name_, symbol_) {
    _creator = _msgSender();
    _mint(_creator, supply_);
    _url = url_;
    _metadata = metadata_;
    _decimals = uint8(decimals_);
  }

  function url() public view returns (string memory) { return _url; }

  function metadata() public view returns (string memory) { return _metadata; }

  function decimals() public view override returns (uint8) { return _decimals; }

  function burn(uint256 amount) public virtual returns (bool) {
    require(_msgSender() == _creator, "must be creator");
    _burn(_creator, amount);
    return true;
  }

  function destroy() public virtual {
    require(_msgSender() == _creator, "must be creator");
    require(totalSupply() == 0, "must be no supply");
    selfdestruct(payable(_creator));
  }
}

// Generated code includes meaning of numbers
error ReachError(uint256 msg);

contract Stdlib {
  function safeAdd(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require((z = x + y) >= x, "add overflow"); }
  function safeSub(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require((z = x - y) <= x, "sub wraparound"); }
  function safeMul(uint256 x, uint256 y) internal pure returns (uint256 z) {
    require(y == 0 || (z = x * y) / y == x, "mul overflow"); }

  function unsafeAdd(uint256 x, uint256 y) internal pure returns (uint256 z) {
    unchecked { z = x + y; } }
  function unsafeSub(uint256 x, uint256 y) internal pure returns (uint256 z) {
    unchecked { z = x - y; } }
  function unsafeMul(uint256 x, uint256 y) internal pure returns (uint256 z) {
    unchecked { z = x * y; } }

  function reachRequire(bool succ, uint256 errMsg) internal pure {
    if ( ! succ ) {
      revert ReachError(errMsg);
    }
  }

  function checkFunReturn(bool succ, bytes memory returnData, uint256 errMsg) internal pure returns (bytes memory) {
    if (succ) {
      return returnData;
    } else {
      if (returnData.length > 0) {
        assembly {
          let returnData_size := mload(returnData)
          revert(add(32, returnData), returnData_size)
        }
      } else {
        revert ReachError(errMsg);
      }
    }
  }

  function tokenAllowance(address payable token, address owner, address spender) internal returns (uint256 amt) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.allowance.selector, owner, spender));
    checkFunReturn(ok, ret, 0 /*'token.allowance'*/);
    amt = abi.decode(ret, (uint256));
  }

  function tokenTransferFrom(address payable token, address sender, address recipient, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.transferFrom.selector, sender, recipient, amt));
    checkFunReturn(ok, ret, 1 /*'token.transferFrom'*/);
    res = abi.decode(ret, (bool));
  }

  function tokenTransfer(address payable token, address recipient, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.transfer.selector, recipient, amt));
    checkFunReturn(ok, ret, 2 /*'token.transfer'*/);
    res = abi.decode(ret, (bool));
  }
  function safeTokenTransfer(address payable token, address recipient, uint256 amt) internal {
    require(tokenTransfer(token, recipient, amt));
  }

  function reachTokenBurn(address payable token, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(ReachToken.burn.selector, amt));
    checkFunReturn(ok, ret, 3 /*'token.burn'*/);
    res = abi.decode(ret, (bool));
  }
  function safeReachTokenBurn(address payable token, uint256 amt) internal {
    require(reachTokenBurn(token, amt));
  }

  function reachTokenDestroy(address payable token) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(ReachToken.destroy.selector));
    checkFunReturn(ok, ret, 4 /*'token.destroy'*/);
    res = true;
  }
  function safeReachTokenDestroy(address payable token) internal {
    require(reachTokenDestroy(token));
  }

  function readPayAmt(address sender, address payable token) internal returns (uint256 amt) {
    amt = tokenAllowance(token, sender, address(this));
    require(checkPayAmt(sender, token, amt));
  }

  function checkPayAmt(address sender, address payable token, uint256 amt) internal returns (bool) {
    return tokenTransferFrom(token, sender, address(this), amt);
  }

  function tokenApprove(address payable token, address spender, uint256 amt) internal returns (bool res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0)}(abi.encodeWithSelector(IERC20.approve.selector, spender, amt));
    checkFunReturn(ok, ret, 5 /*'token.approve'*/);
    res = abi.decode(ret, (bool));
  }

  function tokenBalanceOf(address payable token, address owner) internal returns (uint256 res) {
    (bool ok, bytes memory ret) = token.call{value: uint256(0) }(abi.encodeWithSelector(IERC20.balanceOf.selector, owner));
    checkFunReturn(ok, ret, 6 /*'token.balanceOf'*/);
    res = abi.decode(ret, (uint256));
  }
}

struct T0 {
  bytes32 elem0;
  }
struct T1 {
  bytes8 elem0;
  }
struct T2 {
  address payable v236;
  T0 v237;
  T1 v238;
  uint256 v241;
  uint256 v242;
  address payable v251;
  }
struct T3 {
  bytes32 elem0;
  bytes32 elem1;
  bytes32 elem2;
  }
struct T4 {
  T0 v237;
  T1 v238;
  T3 v239;
  T0 v240;
  uint256 v241;
  uint256 v242;
  }
struct T5 {
  uint256 time;
  T4 msg;
  }
struct T6 {
  address payable v236;
  T0 v237;
  T1 v238;
  uint256 v241;
  uint256 v242;
  address payable v251;
  address payable v254;
  }
struct T8 {
  uint256 time;
  bool msg;
  }
struct T9 {
  address payable v236;
  T0 v237;
  T1 v238;
  uint256 v241;
  address payable v251;
  address payable v254;
  uint256 v265;
  }
struct T10 {
  address payable v236;
  address payable v254;
  uint256 v265;
  address payable v315;
  }
struct T11 {
  address payable v236;
  address payable v254;
  uint256 v265;
  address payable v315;
  uint256 v329;
  }
struct T12 {
  address payable v236;
  address payable v254;
  uint256 v265;
  address payable v315;
  uint256 v348;
  }
struct T13 {
  address payable v254;
  uint256 v265;
  address payable v315;
  uint256 v358;
  }


contract ReachContract is Stdlib {
  uint256 current_step;
  uint256 current_time;
    bytes current_svbs;
  uint256 creation_time;
  function _reachCreationTime() external view returns (uint256) { return creation_time; }
  function _reachCurrentTime() external view returns (uint256) { return current_time; }
  function _reachCurrentState() external view returns (uint256, bytes memory) { return (current_step, current_svbs); }
  
  
  
  event _reach_oe_v250(address payable v0);
  event _reach_oe_v314(address payable v0);
  
  
  event _reach_e0(T5 _a);
  struct _F0 {
    uint256 v246;
    address payable v250;
    address payable v251;
    }
  constructor(T5 memory _a) payable {
    current_step = 0x0;
    creation_time = uint256(block.number);
    _F0 memory _f;
    
    emit _reach_e0(_a);
    reachRequire((msg.value == uint256(0)), uint256(7) /*'(./index.rsh:27:5:dot,[],"verify network token pay amount")'*/);
    _f.v246 = uint256(4) * _a.msg.v242;
    reachRequire(((_f.v246 <= _a.msg.v241)), uint256(8) /*'(./index.rsh:28:10:application,[],Nothing)'*/);
    reachRequire(((_f.v246 <= uint256(115792089237316195423570985008687907853269984665640564039457584007913129639935))), uint256(9) /*'(./index.rsh:29:10:application,[],Nothing)'*/);
    _f.v250 = payable(address(new ReachToken(string(bytes.concat(_a.msg.v237.elem0)), string(bytes.concat(_a.msg.v238.elem0)), string(bytes.concat(_a.msg.v239.elem0, _a.msg.v239.elem1, _a.msg.v239.elem2)), string(bytes.concat(_a.msg.v240.elem0)), _a.msg.v241, uint256(18))));
    _f.v251 = _f.v250;
    emit _reach_oe_v250( _f.v250);
    
    
    T2 memory nsvs;
    nsvs.v236 = payable(msg.sender);
    nsvs.v237 = _a.msg.v237;
    nsvs.v238 = _a.msg.v238;
    nsvs.v241 = _a.msg.v241;
    nsvs.v242 = _a.msg.v242;
    nsvs.v251 = _f.v251;
    current_step = uint256(1);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e1(T8 _a);
  
  function _reach_m1(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(1)), uint256(11) /*'state step check at ./index.rsh:36:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(12) /*'state time check at ./index.rsh:36:5:dot'*/);
    current_step = 0x0;
    (T2 memory _svs) = abi.decode(current_svbs, (T2));
    
    
    emit _reach_e1(_a);
    reachRequire((msg.value == uint256(0)), uint256(10) /*'(./index.rsh:36:5:dot,[],"verify network token pay amount")'*/);
    T6 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v237 = _svs.v237;
    nsvs.v238 = _svs.v238;
    nsvs.v241 = _svs.v241;
    nsvs.v242 = _svs.v242;
    nsvs.v251 = _svs.v251;
    nsvs.v254 = payable(msg.sender);
    current_step = uint256(2);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e2(T8 _a);
  struct _F2 {
    uint256 v265;
    }
  function _reach_m2(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(2)), uint256(15) /*'state step check at ./index.rsh:45:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(16) /*'state time check at ./index.rsh:45:5:dot'*/);
    current_step = 0x0;
    (T6 memory _svs) = abi.decode(current_svbs, (T6));
    _F2 memory _f;
    
    emit _reach_e2(_a);
    reachRequire((msg.value == uint256(0)), uint256(13) /*'(./index.rsh:45:5:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v254 == payable(msg.sender))), uint256(14) /*'(./index.rsh:45:5:dot,[],Just "sender correct")'*/);
    _f.v265 = uint256(2) * _svs.v242;
    safeTokenTransfer(_svs.v251, _svs.v254, _f.v265);
    T9 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v237 = _svs.v237;
    nsvs.v238 = _svs.v238;
    nsvs.v241 = _svs.v241;
    nsvs.v251 = _svs.v251;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _f.v265;
    current_step = uint256(3);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e3(T8 _a);
  
  function _reach_m3(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(3)), uint256(19) /*'state step check at ./index.rsh:48:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(20) /*'state time check at ./index.rsh:48:5:dot'*/);
    current_step = 0x0;
    (T9 memory _svs) = abi.decode(current_svbs, (T9));
    
    
    emit _reach_e3(_a);
    reachRequire((msg.value == uint256(0)), uint256(17) /*'(./index.rsh:48:5:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v236 == payable(msg.sender))), uint256(18) /*'(./index.rsh:48:5:dot,[],Just "sender correct")'*/);
    safeTokenTransfer(_svs.v251, _svs.v236, _svs.v265);
    T9 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v237 = _svs.v237;
    nsvs.v238 = _svs.v238;
    nsvs.v241 = _svs.v241;
    nsvs.v251 = _svs.v251;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _svs.v265;
    current_step = uint256(4);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e4(T8 _a);
  
  function _reach_m4(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(4)), uint256(24) /*'state step check at ./index.rsh:51:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(25) /*'state time check at ./index.rsh:51:5:dot'*/);
    current_step = 0x0;
    (T9 memory _svs) = abi.decode(current_svbs, (T9));
    
    
    emit _reach_e4(_a);
    reachRequire((msg.value == uint256(0)), uint256(21) /*'(./index.rsh:51:5:dot,[],"verify network token pay amount")'*/);
    reachRequire((checkPayAmt(msg.sender, _svs.v251, _svs.v265)), uint256(22) /*'(./index.rsh:51:5:dot,[],"verify non-network token pay amount")'*/);
    reachRequire(((_svs.v236 == payable(msg.sender))), uint256(23) /*'(./index.rsh:51:5:dot,[],Just "sender correct")'*/);
    T9 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v237 = _svs.v237;
    nsvs.v238 = _svs.v238;
    nsvs.v241 = _svs.v241;
    nsvs.v251 = _svs.v251;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _svs.v265;
    current_step = uint256(5);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e5(T8 _a);
  struct _F5 {
    T3 v312;
    T0 v313;
    address payable v314;
    address payable v315;
    }
  function _reach_m5(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(5)), uint256(29) /*'state step check at ./index.rsh:53:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(30) /*'state time check at ./index.rsh:53:5:dot'*/);
    current_step = 0x0;
    (T9 memory _svs) = abi.decode(current_svbs, (T9));
    _F5 memory _f;
    
    emit _reach_e5(_a);
    reachRequire((msg.value == uint256(0)), uint256(26) /*'(./index.rsh:53:5:dot,[],"verify network token pay amount")'*/);
    reachRequire((checkPayAmt(msg.sender, _svs.v251, _svs.v265)), uint256(27) /*'(./index.rsh:53:5:dot,[],"verify non-network token pay amount")'*/);
    reachRequire(((_svs.v254 == payable(msg.sender))), uint256(28) /*'(./index.rsh:53:5:dot,[],Just "sender correct")'*/);
    safeReachTokenBurn(_svs.v251, _svs.v241);
    safeReachTokenDestroy(_svs.v251);
    _f.v312.elem0 = hex'0000000000000000000000000000000000000000000000000000000000000000';
    _f.v312.elem1 = hex'0000000000000000000000000000000000000000000000000000000000000000';
    _f.v312.elem2 = hex'0000000000000000000000000000000000000000000000000000000000000000';
    
    _f.v313.elem0 = hex'0000000000000000000000000000000000000000000000000000000000000000';
    
    _f.v314 = payable(address(new ReachToken(string(bytes.concat(_svs.v237.elem0)), string(bytes.concat(_svs.v238.elem0)), string(bytes.concat(_f.v312.elem0, _f.v312.elem1, _f.v312.elem2)), string(bytes.concat(_f.v313.elem0)), uint256(115792089237316195423570985008687907853269984665640564039457584007913129639935), uint256(18))));
    _f.v315 = _f.v314;
    emit _reach_oe_v314( _f.v314);
    
    
    T10 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _svs.v265;
    nsvs.v315 = _f.v315;
    current_step = uint256(6);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e6(T8 _a);
  struct _F6 {
    uint256 v329;
    }
  function _reach_m6(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(6)), uint256(33) /*'state step check at ./index.rsh:63:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(34) /*'state time check at ./index.rsh:63:5:dot'*/);
    current_step = 0x0;
    (T10 memory _svs) = abi.decode(current_svbs, (T10));
    _F6 memory _f;
    
    emit _reach_e6(_a);
    reachRequire((msg.value == uint256(0)), uint256(31) /*'(./index.rsh:63:5:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v254 == payable(msg.sender))), uint256(32) /*'(./index.rsh:63:5:dot,[],Just "sender correct")'*/);
    _f.v329 = uint256(115792089237316195423570985008687907853269984665640564039457584007913129639935) - _svs.v265;
    safeTokenTransfer(_svs.v315, _svs.v254, _svs.v265);
    T11 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _svs.v265;
    nsvs.v315 = _svs.v315;
    nsvs.v329 = _f.v329;
    current_step = uint256(7);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e7(T8 _a);
  struct _F7 {
    uint256 v341;
    uint256 v348;
    }
  function _reach_m7(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(7)), uint256(37) /*'state step check at ./index.rsh:66:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(38) /*'state time check at ./index.rsh:66:5:dot'*/);
    current_step = 0x0;
    (T11 memory _svs) = abi.decode(current_svbs, (T11));
    _F7 memory _f;
    
    emit _reach_e7(_a);
    reachRequire((msg.value == uint256(0)), uint256(35) /*'(./index.rsh:66:5:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v236 == payable(msg.sender))), uint256(36) /*'(./index.rsh:66:5:dot,[],Just "sender correct")'*/);
    _f.v341 = _svs.v329 - _svs.v265;
    safeTokenTransfer(_svs.v315, _svs.v236, _svs.v265);
    _f.v348 = _f.v341 - _f.v341;
    safeReachTokenBurn(_svs.v315, _f.v341);
    T12 memory nsvs;
    nsvs.v236 = _svs.v236;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _svs.v265;
    nsvs.v315 = _svs.v315;
    nsvs.v348 = _f.v348;
    current_step = uint256(8);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e8(T8 _a);
  struct _F8 {
    uint256 v358;
    }
  function _reach_m8(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(8)), uint256(42) /*'state step check at ./index.rsh:70:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(43) /*'state time check at ./index.rsh:70:5:dot'*/);
    current_step = 0x0;
    (T12 memory _svs) = abi.decode(current_svbs, (T12));
    _F8 memory _f;
    
    emit _reach_e8(_a);
    reachRequire((msg.value == uint256(0)), uint256(39) /*'(./index.rsh:70:5:dot,[],"verify network token pay amount")'*/);
    _f.v358 = _svs.v348 + _svs.v265;
    reachRequire((checkPayAmt(msg.sender, _svs.v315, _svs.v265)), uint256(40) /*'(./index.rsh:70:5:dot,[],"verify non-network token pay amount")'*/);
    reachRequire(((_svs.v236 == payable(msg.sender))), uint256(41) /*'(./index.rsh:70:5:dot,[],Just "sender correct")'*/);
    T13 memory nsvs;
    nsvs.v254 = _svs.v254;
    nsvs.v265 = _svs.v265;
    nsvs.v315 = _svs.v315;
    nsvs.v358 = _f.v358;
    current_step = uint256(9);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e9(T8 _a);
  
  function _reach_m9(T8 calldata _a) external payable {
    reachRequire((current_step == uint256(9)), uint256(47) /*'state step check at ./index.rsh:72:5:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(48) /*'state time check at ./index.rsh:72:5:dot'*/);
    current_step = 0x0;
    (T13 memory _svs) = abi.decode(current_svbs, (T13));
    
    
    emit _reach_e9(_a);
    reachRequire((msg.value == uint256(0)), uint256(44) /*'(./index.rsh:72:5:dot,[],"verify network token pay amount")'*/);
    reachRequire((checkPayAmt(msg.sender, _svs.v315, _svs.v265)), uint256(45) /*'(./index.rsh:72:5:dot,[],"verify non-network token pay amount")'*/);
    reachRequire(((_svs.v254 == payable(msg.sender))), uint256(46) /*'(./index.rsh:72:5:dot,[],Just "sender correct")'*/);
    safeReachTokenBurn(_svs.v315, (_svs.v358 + _svs.v265));
    safeReachTokenDestroy(_svs.v315);
    current_step = 0x0;
    current_time = 0x0;
    delete current_svbs;
    
    
    }
  
  
  
  receive () external payable {}
  fallback () external payable {}
  
  }
