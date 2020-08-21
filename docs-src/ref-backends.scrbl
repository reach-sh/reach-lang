#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends" #:style 'toc]{Participant Backends}

This section describes the @tech{participant} @tech{backends}
supported by Reach version @|reach-vers|.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "ref-backend-js"]{JavaScript}

The Reach JavaScript @tech{backend} produces a compilation output named @filepath{input.export.mjs} which exports an asynchronous function for each @tech{participant}. Each function accepts three arguments: @jsin{stdlib}, @jsin{ctc}, and @jsin{interact}. These functions should be called by the @tech{frontend}.

The @jsin{stdlib} argument is provided by either
@itemlist[
 @item{the module @litchar{@"@"reach-sh/stdlib/ETH.mjs};}
 @item{the module @litchar{@"@"reach-sh/stdlib/ALGO.mjs}; or,} 
 @item{the module @litchar{@"@"reach-sh/stdlib/FAKE.mjs}.} ]

The @jsin{ctc} argument is the result of a call to @jsin{acc.deploy} or @jsin{acc.attach}.

The @jsin{interact} argument is an object matching the @tech{participant interact interface} for the corresponding @tech{participant}.

@(hrule)

This backend does not guarantee that values in a positive position in a @tech{participant interact interface}, that are later passed to a negative position in a @tech{participant interact interface}, will be identical, in the sense of JavaScript's @jsin{===} operator, to the original value.
In other words, this backend does not ensure that Reach programs are parametric over JavaScript values that they interact with.

Positive and negative are best understood by example with a function type: a positive position is supplied by the function, such as the result; while a negative position is supplied by the caller, such as the arguments.
These notions generalize, however, to higher (and lower) order contexts.
In the case of Reach, this means that non-function values in @tech{participant interact interface} are positive.

For example, if the Reach program,

@reach{
 Reach.App({},
  [["A", { get: Bytes, give: Fun([Bytes], Bool) }]],
  (A) => {
   A.only(() => {
    const x = interact.give(interact.get); });
   A.publish(x);
   commit(); }); }

is given the @jsin{interact} object,

@js{
 const x = "A string";
 { get: x,
   give: (str) => x === str } }

then it is not guaranteed that @reachin{A} will publish @reachin{true}, because the @jsin{str} given to @jsin{give} may not be identical to @jsin{x}.

@(hrule)

The @jsin{stdlib} modules export the following functions that might be used in this @tech{frontend}:

@(hrule)
@js{
 async newTestAccount(balance) => acc }

@index{newTestAccount} Returns a Reach @tech{account} abstraction for a new @tech{account} on the @tech{consensus network} with a given balance of @tech{network tokens}. This can only be used in private testing scenarios, as it uses a private faucet to issue @tech{network tokens}.

@(hrule)
@js{
 async connectAccount(networkAccount) => acc }

@index{connectAccount} Returns a Reach @tech{account} abstraction for an existing @tech{account} for the @tech{consensus network} based on the @tech{connector}-specific @tech{account} specification provided by the @jsin{networkAccount} argument.

@js{
    // network => networkAccount type
    ETH        => ethers.Wallet
    ALGO       => {addr: string, sk: UInt8Array(64)}}

@(hrule)
@js{
 acc.networkAccount => networkAccount }

@index{acc.networkAccount} Returns the @tech{connector}-specific @tech{account} specification of a Reach @tech{account} abstraction.

@(hrule)
@js{
 async acc.deploy(bin) => ctc }

@index{acc.deploy} Returns a Reach @tech{contract} abstraction after deploying a Reach @DApp @tech{contract} based on the @jsin{bin} argument provided. This @jsin{bin} argument is the @filepath{input.mjs} module produced by the JavaScript @tech{backend}.

@(hrule)
@js{
 async acc.attach(bin, ctc) => ctc }

@index{acc.attach} Returns a Reach @tech{contract} abstraction based on a deployed Reach @DApp @tech{contract} provided in the @jsin{ctc} argument and the @jsin{bin} argument. This @jsin{bin} argument is the @filepath{input.mjs} module produced by the JavaScript @tech{backend}.

@(hrule)
@js{
 async balanceOf(acc) => amount }

@index{balanceOf} Returns the balance of @tech{network tokens} held by the @tech{account} given by a Reach @tech{account} abstraction provided by the @jsin{acc} argument.

@(hrule)
@js{
 async transfer(from:networkAccount, to:networkAccount, amount) => void }

@index{transfer} Transfers @jsin{amount} @tech{network tokens} from @jsin{from} to @jsin{to}, which are provided by @tech{connector}-specific @tech{account} specifications.

@(hrule)
@js{
 protect(t x) => x }

@index{protect} Asserts that value @jsin{x} has Reach @tech{type} @jsin{t}. An exception is thrown if this is not the case.

@(hrule)
@js{
 T_Null => ReachType
 T_Bool => ReachType
 T_UInt256 => ReachType
 T_Bytes => ReachType
 T_Address => ReachType
 T_Array(ReachType, number) => ReachType
 T_Tuple([ReachType ...]) => ReachType
 T_Obj({Key: ReachType ...}) => ReachType}

Each of these represent the corresponding Reach @tech{type}.
See the table below for Reach types and their corresponding JavaScript representation:

@js{
 // Reach  => JavaScript
 Null      => null
 Bool      => 'boolean'
 UInt256   => 'BigNumber' or 'number'
 Bytes     => 'string'
 Address   => 'string'
 Array     => array
 Tuple     => array
 Object    => object }

@(hrule)
@js{
 assert(p) }

Throws an exception if not given @jsin{true}.

@(hrule)
@js{
 array_set(arr, idx, val) }

Returns a new array identical to @jsin{arr}, except that index @jsin{idx} is @jsin{val}.

@(hrule)
@js{
 bigNumberify(x) => uint256
 isBigNumber(x) => bool}

@deftech{bigNumberify} converts a JavaScript number to a BigNumber,
the JavaScript representation of Reach's uint256.
@deftech{isBigNumber} checks if its input is a BigNumber.

@(hrule)
@js{
 toHex(x) => bytes
 isHex(x) => bool
 hexToString(bytes) => string
 hexToBigNumber(bytes) => uint256
 bigNumberToHex(uint256) => bytes
 uint256_to_bytes(uint256) => bytes
 bytes_eq(bytes, bytes) => bool }

These are additional conversion and comparison utilities.

@(hrule)
@js{
 keccak256(x) => uint256}

@index{keccak256} Hashes the value.

@(hrule)
@js{
 random_uint256() => uint256}

@index{random_uint256} Creates 256 random bits as a uint256.

@(hrule)
@js{
 hasRandom}

A value suitable for use as a @tech{participant interact interface} requiring a @litchar{random} function.

@(hrule)
@js{
 add(uint256, uint256) => uint256
 sub(uint256, uint256) => uint256
 mod(uint256, uint256) => uint256
 mul(uint256, uint256) => uint256
 div(uint256, uint256) => uint256 }

Integer arithmetic on uint256.

@(hrule)
@js{
 eq(uint256, uint256) => bool
 ge(uint256, uint256) => bool
 gt(uint256, uint256) => bool
 le(uint256, uint256) => bool
 lt(uint256, uint256) => bool}

Integer comparisons on uint256.

@(hrule)
@js{
 toWei(ether) => wei
 fromWei(wei) => ether
 toWeiBigNumber(ether) => uint256}

Wei conversion functions only exported by the stdlib for ETH.

