// Automatically generated with Reach 0.1.10 (f1275a40*)
pragma abicoder v2;

pragma solidity ^0.8.12;

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
     * @dev Moves `amount` tokens from the caller's account to `to`.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    function transfer(address to, uint256 amount) external returns (bool);

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
     * @dev Moves `amount` tokens from `from` to `to` using the
     * allowance mechanism. `amount` is then deducted from the caller's
     * allowance.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    function transferFrom(
        address from,
        address to,
        uint256 amount
    ) external returns (bool);

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

/**
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
 * We have followed general OpenZeppelin Contracts guidelines: functions revert
 * instead returning `false` on failure. This behavior is nonetheless
 * conventional and does not conflict with the expectations of ERC20
 * applications.
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
    mapping(address => uint256) private _balances;

    mapping(address => mapping(address => uint256)) private _allowances;

    uint256 private _totalSupply;

    string private _name;
    string private _symbol;

    /**
     * @dev Sets the values for {name} and {symbol}.
     *
     * The default value of {decimals} is 18. To select a different value for
     * {decimals} you should overload it.
     *
     * All two of these values are immutable: they can only be set once during
     * construction.
     */
    constructor(string memory name_, string memory symbol_) {
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
     * be displayed to a user as `5.05` (`505 / 10 ** 2`).
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
     * - `to` cannot be the zero address.
     * - the caller must have a balance of at least `amount`.
     */
    function transfer(address to, uint256 amount) public virtual override returns (bool) {
        address owner = _msgSender();
        _transfer(owner, to, amount);
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
     * NOTE: If `amount` is the maximum `uint256`, the allowance is not updated on
     * `transferFrom`. This is semantically equivalent to an infinite approval.
     *
     * Requirements:
     *
     * - `spender` cannot be the zero address.
     */
    function approve(address spender, uint256 amount) public virtual override returns (bool) {
        address owner = _msgSender();
        _approve(owner, spender, amount);
        return true;
    }

    /**
     * @dev See {IERC20-transferFrom}.
     *
     * Emits an {Approval} event indicating the updated allowance. This is not
     * required by the EIP. See the note at the beginning of {ERC20}.
     *
     * NOTE: Does not update the allowance if the current allowance
     * is the maximum `uint256`.
     *
     * Requirements:
     *
     * - `from` and `to` cannot be the zero address.
     * - `from` must have a balance of at least `amount`.
     * - the caller must have allowance for ``from``'s tokens of at least
     * `amount`.
     */
    function transferFrom(
        address from,
        address to,
        uint256 amount
    ) public virtual override returns (bool) {
        address spender = _msgSender();
        _spendAllowance(from, spender, amount);
        _transfer(from, to, amount);
        return true;
    }

    /**
     * @dev Atomically increases the allowance granted to `spender` by the caller.
     *
     * This is an alternative to {approve} that can be used as a mitigation for
     * problems described in {IERC20-approve}.
     *
     * Emits an {Approval} event indicating the updated allowance.
     *
     * Requirements:
     *
     * - `spender` cannot be the zero address.
     */
    function increaseAllowance(address spender, uint256 addedValue) public virtual returns (bool) {
        address owner = _msgSender();
        _approve(owner, spender, _allowances[owner][spender] + addedValue);
        return true;
    }

    /**
     * @dev Atomically decreases the allowance granted to `spender` by the caller.
     *
     * This is an alternative to {approve} that can be used as a mitigation for
     * problems described in {IERC20-approve}.
     *
     * Emits an {Approval} event indicating the updated allowance.
     *
     * Requirements:
     *
     * - `spender` cannot be the zero address.
     * - `spender` must have allowance for the caller of at least
     * `subtractedValue`.
     */
    function decreaseAllowance(address spender, uint256 subtractedValue) public virtual returns (bool) {
        address owner = _msgSender();
        uint256 currentAllowance = _allowances[owner][spender];
        require(currentAllowance >= subtractedValue, "ERC20: decreased allowance below zero");
        unchecked {
            _approve(owner, spender, currentAllowance - subtractedValue);
        }

        return true;
    }

    /**
     * @dev Moves `amount` of tokens from `sender` to `recipient`.
     *
     * This internal function is equivalent to {transfer}, and can be used to
     * e.g. implement automatic token fees, slashing mechanisms, etc.
     *
     * Emits a {Transfer} event.
     *
     * Requirements:
     *
     * - `from` cannot be the zero address.
     * - `to` cannot be the zero address.
     * - `from` must have a balance of at least `amount`.
     */
    function _transfer(
        address from,
        address to,
        uint256 amount
    ) internal virtual {
        require(from != address(0), "ERC20: transfer from the zero address");
        require(to != address(0), "ERC20: transfer to the zero address");

        _beforeTokenTransfer(from, to, amount);

        uint256 fromBalance = _balances[from];
        require(fromBalance >= amount, "ERC20: transfer amount exceeds balance");
        unchecked {
            _balances[from] = fromBalance - amount;
        }
        _balances[to] += amount;

        emit Transfer(from, to, amount);

        _afterTokenTransfer(from, to, amount);
    }

    /** @dev Creates `amount` tokens and assigns them to `account`, increasing
     * the total supply.
     *
     * Emits a {Transfer} event with `from` set to the zero address.
     *
     * Requirements:
     *
     * - `account` cannot be the zero address.
     */
    function _mint(address account, uint256 amount) internal virtual {
        require(account != address(0), "ERC20: mint to the zero address");

        _beforeTokenTransfer(address(0), account, amount);

        _totalSupply += amount;
        _balances[account] += amount;
        emit Transfer(address(0), account, amount);

        _afterTokenTransfer(address(0), account, amount);
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

        _beforeTokenTransfer(account, address(0), amount);

        uint256 accountBalance = _balances[account];
        require(accountBalance >= amount, "ERC20: burn amount exceeds balance");
        unchecked {
            _balances[account] = accountBalance - amount;
        }
        _totalSupply -= amount;

        emit Transfer(account, address(0), amount);

        _afterTokenTransfer(account, address(0), amount);
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
    function _approve(
        address owner,
        address spender,
        uint256 amount
    ) internal virtual {
        require(owner != address(0), "ERC20: approve from the zero address");
        require(spender != address(0), "ERC20: approve to the zero address");

        _allowances[owner][spender] = amount;
        emit Approval(owner, spender, amount);
    }

    /**
     * @dev Spend `amount` form the allowance of `owner` toward `spender`.
     *
     * Does not update the allowance amount in case of infinite allowance.
     * Revert if not enough allowance is available.
     *
     * Might emit an {Approval} event.
     */
    function _spendAllowance(
        address owner,
        address spender,
        uint256 amount
    ) internal virtual {
        uint256 currentAllowance = allowance(owner, spender);
        if (currentAllowance != type(uint256).max) {
            require(currentAllowance >= amount, "ERC20: insufficient allowance");
            unchecked {
                _approve(owner, spender, currentAllowance - amount);
            }
        }
    }

    /**
     * @dev Hook that is called before any transfer of tokens. This includes
     * minting and burning.
     *
     * Calling conditions:
     *
     * - when `from` and `to` are both non-zero, `amount` of ``from``'s tokens
     * will be transferred to `to`.
     * - when `from` is zero, `amount` tokens will be minted for `to`.
     * - when `to` is zero, `amount` of ``from``'s tokens will be burned.
     * - `from` and `to` are never both zero.
     *
     * To learn more about hooks, head to xref:ROOT:extending-contracts.adoc#using-hooks[Using Hooks].
     */
    function _beforeTokenTransfer(
        address from,
        address to,
        uint256 amount
    ) internal virtual {}

    /**
     * @dev Hook that is called after any transfer of tokens. This includes
     * minting and burning.
     *
     * Calling conditions:
     *
     * - when `from` and `to` are both non-zero, `amount` of ``from``'s tokens
     * has been transferred to `to`.
     * - when `from` is zero, `amount` tokens have been minted for `to`.
     * - when `to` is zero, `amount` of ``from``'s tokens have been burned.
     * - `from` and `to` are never both zero.
     *
     * To learn more about hooks, head to xref:ROOT:extending-contracts.adoc#using-hooks[Using Hooks].
     */
    function _afterTokenTransfer(
        address from,
        address to,
        uint256 amount
    ) internal virtual {}
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
  address payable v304;
  uint256 v305;
  uint256 v306;
  uint256 v317;
  }
struct T1 {
  uint256 v305;
  uint256 v306;
  }
struct T2 {
  uint256 time;
  T1 msg;
  }
struct T3 {
  address payable v304;
  uint256 v305;
  uint256 v306;
  address payable v321;
  }
struct T4 {
  uint256 v326;
  uint256 v327;
  uint256 v334;
  }
struct T5 {
  T3 svs;
  T4 msg;
  }
struct T7 {
  uint256 time;
  bool msg;
  }
struct T8 {
  address payable v304;
  uint256 v305;
  uint256 v306;
  address payable v321;
  uint256 v334;
  uint256 v349;
  }
struct T9 {
  address payable v304;
  uint256 v305;
  uint256 v306;
  address payable v321;
  uint256 v334;
  uint256 v358;
  uint256 v368;
  }
struct T10 {
  uint256 v358;
  }
struct T11 {
  uint256 time;
  T10 msg;
  }
struct T12 {
  address payable v304;
  uint256 v305;
  uint256 v306;
  address payable v321;
  uint256 v334;
  uint256 v358;
  uint256 v374;
  uint256 v384;
  }
struct T13 {
  uint256 v374;
  }
struct T14 {
  uint256 time;
  T13 msg;
  }
struct T15 {
  uint256 v389;
  uint256 v390;
  }
struct T16 {
  uint256 time;
  T15 msg;
  }


contract ReachContract is Stdlib {
  uint256 current_step;
  uint256 current_time;
    bytes current_svbs;
  uint256 creation_time;
  function _reachCreationTime() external view returns (uint256) { return creation_time; }
  function _reachCurrentTime() external view returns (uint256) { return current_time; }
  function _reachCurrentState() external view returns (uint256, bytes memory) { return (current_step, current_svbs); }
  
  
  
  
  
  event _reach_e0(address _who, T2 _a);
  struct _F0 {
    uint256 v317;
    }
  constructor(T2 memory _a) payable {
    current_step = 0x0;
    creation_time = uint256(block.number);
    _F0 memory _f;
    
    emit _reach_e0(msg.sender, _a);
    reachRequire((msg.value == _a.msg.v305), uint256(7) /*'(./examples/rps-8-interact/index.rsh:49:9:dot,[],"verify network token pay amount")'*/);
    _f.v317 = uint256(block.number) + _a.msg.v306;
    T0 memory nsvs;
    nsvs.v304 = payable(msg.sender);
    nsvs.v305 = _a.msg.v305;
    nsvs.v306 = _a.msg.v306;
    nsvs.v317 = _f.v317;
    current_step = uint256(1);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e1(address _who, T7 _a);
  
  function _reach_m1(T7 calldata _a) external payable {
    reachRequire((current_step == uint256(1)), uint256(9) /*'state step check at ./examples/rps-8-interact/index.rsh:56:7:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(10) /*'state time check at ./examples/rps-8-interact/index.rsh:56:7:dot'*/);
    current_step = 0x0;
    (T0 memory _svs) = abi.decode(current_svbs, (T0));
    
    reachRequire((uint256(block.number) < _svs.v317), uint256(11) /*'timeout check at ./examples/rps-8-interact/index.rsh:56:7:dot'*/);
    
    emit _reach_e1(msg.sender, _a);
    reachRequire((msg.value == _svs.v305), uint256(8) /*'(./examples/rps-8-interact/index.rsh:56:7:dot,[],"verify network token pay amount")'*/);
    T5 memory la;
    la.svs.v304 = _svs.v304;
    la.svs.v305 = _svs.v305;
    la.svs.v306 = _svs.v306;
    la.svs.v321 = payable(msg.sender);
    la.msg.v326 = uint256(1);
    la.msg.v327 = uint256(block.number);
    la.msg.v334 = (_svs.v305 + _svs.v305);
    l3(la);
    
    
    }
  
  event _reach_e2(address _who, T7 _a);
  
  function _reach_m2(T7 calldata _a) external payable {
    reachRequire((current_step == uint256(1)), uint256(13) /*'state step check at reach standard library:189:11:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(14) /*'state time check at reach standard library:189:11:dot'*/);
    current_step = 0x0;
    (T0 memory _svs) = abi.decode(current_svbs, (T0));
    
    reachRequire((uint256(block.number) >= _svs.v317), uint256(15) /*'timeout check at reach standard library:189:11:dot'*/);
    
    emit _reach_e2(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(12) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:57:51:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],"verify network token pay amount")'*/);
    _svs.v304.transfer(_svs.v305);
    current_step = 0x0;
    current_time = 0x0;
    delete current_svbs;
    
    
    }
  
  struct _F3 {
    uint256 v349;
    }
  function l3(T5 memory _a)  internal {
    _F3 memory _f;
    
    if ((_a.msg.v326 == uint256(1))) {
      _f.v349 = _a.msg.v327 + _a.svs.v306;
      T8 memory nsvs;
      nsvs.v304 = _a.svs.v304;
      nsvs.v305 = _a.svs.v305;
      nsvs.v306 = _a.svs.v306;
      nsvs.v321 = _a.svs.v321;
      nsvs.v334 = _a.msg.v334;
      nsvs.v349 = _f.v349;
      current_step = uint256(5);
      current_time = uint256(block.number);
      current_svbs = abi.encode(nsvs);
      }
    else {
      ((_a.msg.v326 == uint256(2)) ? _a.svs.v304 : _a.svs.v321).transfer((uint256(2) * _a.svs.v305));
      current_step = 0x0;
      current_time = 0x0;
      delete current_svbs;
      }
    
    }
  
  event _reach_e4(address _who, T11 _a);
  struct _F4 {
    uint256 v368;
    }
  function _reach_m4(T11 calldata _a) external payable {
    reachRequire((current_step == uint256(5)), uint256(18) /*'state step check at ./examples/rps-8-interact/index.rsh:69:11:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(19) /*'state time check at ./examples/rps-8-interact/index.rsh:69:11:dot'*/);
    current_step = 0x0;
    (T8 memory _svs) = abi.decode(current_svbs, (T8));
    _F4 memory _f;
    reachRequire((uint256(block.number) < _svs.v349), uint256(20) /*'timeout check at ./examples/rps-8-interact/index.rsh:69:11:dot'*/);
    
    emit _reach_e4(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(16) /*'(./examples/rps-8-interact/index.rsh:69:11:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v304 == payable(msg.sender))), uint256(17) /*'(./examples/rps-8-interact/index.rsh:69:11:dot,[],Just "sender correct")'*/);
    _f.v368 = uint256(block.number) + _svs.v306;
    T9 memory nsvs;
    nsvs.v304 = _svs.v304;
    nsvs.v305 = _svs.v305;
    nsvs.v306 = _svs.v306;
    nsvs.v321 = _svs.v321;
    nsvs.v334 = _svs.v334;
    nsvs.v358 = _a.msg.v358;
    nsvs.v368 = _f.v368;
    current_step = uint256(7);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e5(address _who, T7 _a);
  
  function _reach_m5(T7 calldata _a) external payable {
    reachRequire((current_step == uint256(5)), uint256(23) /*'state step check at reach standard library:189:11:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(24) /*'state time check at reach standard library:189:11:dot'*/);
    current_step = 0x0;
    (T8 memory _svs) = abi.decode(current_svbs, (T8));
    
    reachRequire((uint256(block.number) >= _svs.v349), uint256(25) /*'timeout check at reach standard library:189:11:dot'*/);
    
    emit _reach_e5(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(21) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],"verify network token pay amount")'*/);
    reachRequire((((_svs.v304 == payable(msg.sender)) ? true : (_svs.v321 == payable(msg.sender)))), uint256(22) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:70:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],Just "sender correct")'*/);
    _svs.v321.transfer(_svs.v334);
    current_step = 0x0;
    current_time = 0x0;
    delete current_svbs;
    
    
    }
  
  event _reach_e6(address _who, T14 _a);
  struct _F6 {
    uint256 v384;
    }
  function _reach_m6(T14 calldata _a) external payable {
    reachRequire((current_step == uint256(7)), uint256(28) /*'state step check at ./examples/rps-8-interact/index.rsh:77:9:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(29) /*'state time check at ./examples/rps-8-interact/index.rsh:77:9:dot'*/);
    current_step = 0x0;
    (T9 memory _svs) = abi.decode(current_svbs, (T9));
    _F6 memory _f;
    reachRequire((uint256(block.number) < _svs.v368), uint256(30) /*'timeout check at ./examples/rps-8-interact/index.rsh:77:9:dot'*/);
    
    emit _reach_e6(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(26) /*'(./examples/rps-8-interact/index.rsh:77:9:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v321 == payable(msg.sender))), uint256(27) /*'(./examples/rps-8-interact/index.rsh:77:9:dot,[],Just "sender correct")'*/);
    _f.v384 = uint256(block.number) + _svs.v306;
    T12 memory nsvs;
    nsvs.v304 = _svs.v304;
    nsvs.v305 = _svs.v305;
    nsvs.v306 = _svs.v306;
    nsvs.v321 = _svs.v321;
    nsvs.v334 = _svs.v334;
    nsvs.v358 = _svs.v358;
    nsvs.v374 = _a.msg.v374;
    nsvs.v384 = _f.v384;
    current_step = uint256(9);
    current_time = uint256(block.number);
    current_svbs = abi.encode(nsvs);
    
    
    }
  
  event _reach_e7(address _who, T7 _a);
  
  function _reach_m7(T7 calldata _a) external payable {
    reachRequire((current_step == uint256(7)), uint256(33) /*'state step check at reach standard library:189:11:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(34) /*'state time check at reach standard library:189:11:dot'*/);
    current_step = 0x0;
    (T9 memory _svs) = abi.decode(current_svbs, (T9));
    
    reachRequire((uint256(block.number) >= _svs.v368), uint256(35) /*'timeout check at reach standard library:189:11:dot'*/);
    
    emit _reach_e7(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(31) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],"verify network token pay amount")'*/);
    reachRequire((((_svs.v304 == payable(msg.sender)) ? true : (_svs.v321 == payable(msg.sender)))), uint256(32) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:78:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],Just "sender correct")'*/);
    _svs.v304.transfer(_svs.v334);
    current_step = 0x0;
    current_time = 0x0;
    delete current_svbs;
    
    
    }
  
  event _reach_e8(address _who, T16 _a);
  
  function _reach_m8(T16 calldata _a) external payable {
    reachRequire((current_step == uint256(9)), uint256(39) /*'state step check at ./examples/rps-8-interact/index.rsh:85:11:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(40) /*'state time check at ./examples/rps-8-interact/index.rsh:85:11:dot'*/);
    current_step = 0x0;
    (T12 memory _svs) = abi.decode(current_svbs, (T12));
    
    reachRequire((uint256(block.number) < _svs.v384), uint256(41) /*'timeout check at ./examples/rps-8-interact/index.rsh:85:11:dot'*/);
    
    emit _reach_e8(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(36) /*'(./examples/rps-8-interact/index.rsh:85:11:dot,[],"verify network token pay amount")'*/);
    reachRequire(((_svs.v304 == payable(msg.sender))), uint256(37) /*'(./examples/rps-8-interact/index.rsh:85:11:dot,[],Just "sender correct")'*/);
    reachRequire(((_svs.v358 == (uint256(keccak256(abi.encode(_a.msg.v389, _a.msg.v390)))))), uint256(38) /*'(reach standard library:58:17:application,[at ./examples/rps-8-interact/index.rsh:87:20:application call to "checkCommitment" (defined at: reach standard library:57:8:function exp)],Nothing)'*/);
    T5 memory la;
    la.svs.v304 = _svs.v304;
    la.svs.v305 = _svs.v305;
    la.svs.v306 = _svs.v306;
    la.svs.v321 = _svs.v321;
    la.msg.v326 = ((_a.msg.v390 + (uint256(4) - _svs.v374)) % uint256(3));
    la.msg.v327 = uint256(block.number);
    la.msg.v334 = _svs.v334;
    l3(la);
    
    
    }
  
  event _reach_e9(address _who, T7 _a);
  
  function _reach_m9(T7 calldata _a) external payable {
    reachRequire((current_step == uint256(9)), uint256(44) /*'state step check at reach standard library:189:11:dot'*/);
    reachRequire(((_a.time == uint256(0)) || (current_time == _a.time)), uint256(45) /*'state time check at reach standard library:189:11:dot'*/);
    current_step = 0x0;
    (T12 memory _svs) = abi.decode(current_svbs, (T12));
    
    reachRequire((uint256(block.number) >= _svs.v384), uint256(46) /*'timeout check at reach standard library:189:11:dot'*/);
    
    emit _reach_e9(msg.sender, _a);
    reachRequire((msg.value == uint256(0)), uint256(42) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],"verify network token pay amount")'*/);
    reachRequire((((_svs.v304 == payable(msg.sender)) ? true : (_svs.v321 == payable(msg.sender)))), uint256(43) /*'(reach standard library:189:11:dot,[at ./examples/rps-8-interact/index.rsh:86:53:application call to "closeTo" (defined at: reach standard library:187:8:function exp)],Just "sender correct")'*/);
    _svs.v321.transfer(_svs.v334);
    current_step = 0x0;
    current_time = 0x0;
    delete current_svbs;
    
    
    }
  
  
  
  receive () external payable {}
  fallback () external payable {}
  
  }
