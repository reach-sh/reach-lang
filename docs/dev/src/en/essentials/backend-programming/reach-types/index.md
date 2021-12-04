---
menuItem: mi-docs
---

# Reach Types

Reach supports the following data types in *index.rsh* files. Use [Type](/en/essentials/backend-programming/reach-operators/#type-operators) operators and functions to inspect the data types of values.

# Address

An `Address` represents an account address. The `Address` format differs slightly depending on the consensus network. The following example reports the account address of a participant:

``` js nonum
// index.rsh
const myInteract = {
  reportAddress: Fun([Address], Null)
};
M.interact.reportAddress(this);
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = {
  reportAddress: (v) => { console.log(v); }
};
```

And, here is the output:

``` nonum
# REACH_CONNECTOR_MODE=ALGO-devnet
0x5b26ec467ab3e54c793315b548923ec376624d3d29e70083bd2555ef24da8383
  
# REACH_CONNECTOR_MODE=CFX-devnet
NET999:ACD8UE5KYBVG62MB9Z8R20CFBPHJ632ESJTEKUZ31V
  
# REACH_CONNECTOR_MODE=ETH-devnet
0xD2651496526c4E93fc01A87F99bD38092A244FB5
```

The `addressEq` function compares two addresses. The following example compares the addresses of a participant and the contract:

``` js nonum
// index.rsh
const myInteract = {
  reportBool: (v) => { console.log(v); }
};
M.interact.reportBool(addressEq(getAddress(), this));
```

The operators `==`, `===`, `!=`, `!==` are also valid:

``` js nonum
// index.rsh
M.interact.reportBool(getAddress() == this);
```

# API

An `API` expression defines an API in the contract:

``` js nonum
export const main = Reach.App(() => {
  const P = Participant('Participant', {});
  const V = View('Viewer', {});
  const A = API('Writer', {
    writeN: Fun([UInt], State),
    writeT: Fun([UInt], State),
    writeB: Fun([UInt], State),
    writeX: Fun([UInt], State)
  });
  deploy();
  //...
  exit();
});
```

`API` arguments include a name and an interface comprised of methods available in frontends via the `ctc.apis` object. The return value is an object whose fields are the interface methods. The object may be used in the `.api` component of [fork](/en/essentials/backend-programming/reach-statements/#fork) and [parallelReduce](/en/essentials/backend-programming/reach-statements/#parallelreduce) expressions. Each method must occur exactly once in the entire program. See [this example](https://github.com/reach-sh/reach-lang/blob/master/examples/api-full/index.rsh).

# Array

An `Array` is an immutable, ordered list of values. The size of an array is fixed, and the values must be of the same type. Values may be referenced using a zero-based index. The following creates an array of type `Array(UInt, 3)` from a tuple:

``` js nonum
const a = array(UInt, [4, 6, 8]);
const v = a[0]; // 4
```

The following creates an array of type `Array(Object({price: UInt, quantity: UInt}), 3)` from a tuple:

``` js nonum
const a = array(
  Object({price: UInt, quantity: UInt}),
  [
    {price:1000, quantity:10},
    {price:2000, quantity:20},
    {price:3000, quantity:30}
  ]
);
const p = a[0].price; // 1000
```

### Array.all

This method returns `true` if every element in the array satisfies the given `Bool` function. Otherwise, it returns `false`. 

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportBool(Array.all(a, e => e > 0)); // true
// or
M.interact.reportBool(a.all(e => e > 0)); // true
```

### Array.and

This method returns `true` if all elements in a `Bool` array are `true`. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) };
const a = array(Bool, [true, false, true]);
M.interact.reportBool(Array.and(a)); // false
```

### Array.any

This method returns `true` if at least one element in the array satisfies the given `Bool` function. Otherwise, it returns `false`. 

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportBool(Array.any(a, e => e > 7)); // true
// or
M.interact.reportBool(a.any(e => e > 7)); // true
```

### Array.average

This method returns the mean of an array of `UInts`.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.average(a)); // 6
// or
M.interact.reportUInt(a.average()); // 6
```

### Array.concat

This method returns a new array representing the concatenation of the two given arrays.

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 6)], Null) };
const a = array(UInt, [4, 6, 8]);
const b = array(UInt, [10, 12, 14]);
M.interact.reportUIntArray(Array.concat(a, b)); // array(UInt, [4, 6, 8, 10, 12, 14])
// or
M.interact.reportUIntArray(a.concat(b)); // array(UInt, [4, 6, 8, 10, 12, 14])
```

### Array.count

This method returns the number of elements in the array that satisfy the given `Bool` function.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.count(a, e => e > 5)); // 2
// or
M.interact.reportUInt(a.count(e => e > 5)); // 2
```

### Array.elemType

This method returns the type of element that the array contains.

``` js nonum
// index.rsh
const a = array(UInt, [4, 6, 8]);
const t = Array.elemType(a);
// or
const t = a.elemType;
```

### Array.empty

`Array.empty` is an array with no elements. It is the [identity element](https://en.wikipedia.org/wiki/Identity_element) of `Array.concat`. It may also be written `Array_empty`.

### Array.find

This method returns the first element in the array that satisfies the given `Bool` function. The return value is of type [Data](#data). 

``` js nonum
// index.rsh
const myInteract = { reportData: Fun([Data({"None": Null, "Some": UInt})], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportData(Array.find(a, e => e > 3)); // Some,4
// or
M.interact.reportData(a.find(e => e > 3)); // Some,4
```

### Array.findIndex

This method returns the index of the first element in the array that satisfies the given `Bool` function. The return value is of type [Data](#data). 

``` js nonum
// index.rsh
const myInteract = { reportData: Fun([Data({"None": Null, "Some": UInt})], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportData(Array.findIndex(a, e => e > 5)); // Some,1
// or
M.interact.reportData(a.findIndex(e => e > 5)); // Some,1
```

### Array.forEach

This method iterates the specified function over the elements of the array and returns `Null`. It is often used to transfer funds to accounts:

``` js nonum
// index.rsh
// Assume that sale.arbitrator is Array(Address, 3)
const arbFee = (buyerPmt * (2 / 100)) / Array.length(sale.arbitrator);
Array.forEach(sale.arbitrator, (a) => {
  transfer(arbFee).to(a);
});
transfer(balance()).to(Owner);
```

### Array.includes

This method returns `true` if the array contains the specified element. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportBool(Array.includes(a, 4)); // true
// or
M.interact.reportBool(a.includes(4)); // true
```

### Array.indexOf

This method returns the index of the first element in the array that equals the given value. The return value is of type [Data](#data). 

``` js nonum
// index.rsh
const myInteract = { reportData: Fun([Data({"None": Null, "Some": UInt})], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportData(Array.indexOf(a, 6)); // Some,1
// or
M.interact.reportData(a.indexOf(6)); // Some,1
```

### Array.iota

This method returns an array of the specified length where each element is the same as its index.

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 3)], Null), };
M.interact.reportUIntArray(Array.iota(3)); // array(UInt, [0, 1, 2])
```

### Array.length

This method returns the length of the array.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.length(a)); // 3
// or
M.interact.reportUInt(a.length); // 3
```

### Array.map

This method applies the given function to each element of the given array, and returns a new same-length array containing the transformed values.

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 3)], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUIntArray(Array.map(a, e => e + 1)); // array(UInt, [5, 7, 9])
// or
M.interact.reportUIntArray(a.map(e => e + 1)); // array(UInt, [5, 7, 9])
// also
M.interact.reportUIntArray(Array.iota(3).map(a, add)); // array(UInt, [4, 7, 10])
```

### Array.mapWithIndex

This method is similar to `Array.map` except that it provides the function with the element's index, too.

``` js nonum
// index.rsh
const myInteract = { reportObjectArray: Fun([Array(Object({value: UInt, index: UInt}), 3)], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportObjectArray(Array.mapWithIndex(a, (e, i)  => {return {"value": e + 1, "index": i}}));
// or
M.interact.reportObjectArray(a.mapWithIndex((e, i)  => {return {"value": e + 1, "index": i}}));
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
reportObjectArray: (v) => { console.log(v); }
```

Here is the output:

``` nonum
[
  {
    index: BigNumber { _hex: '0x00', _isBigNumber: true },
    value: BigNumber { _hex: '0x05', _isBigNumber: true }
  },
  {
    index: BigNumber { _hex: '0x01', _isBigNumber: true },
    value: BigNumber { _hex: '0x07', _isBigNumber: true }
  },
  {
    index: BigNumber { _hex: '0x02', _isBigNumber: true },
    value: BigNumber { _hex: '0x09', _isBigNumber: true }
  }
]
```

### Array.max

This method returns the largest number in an array of unsigned integers:

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.max(a)); // 8
// or
M.interact.reportUInt(a.max()); // 8
```

### Array.min

This method returns the smallest number in an array of unsigned integers:

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.min(a)); // 4
// or
M.interact.reportUInt(a.min()); // 4
```

### Array.or

This method returns `true` if any elements in a `Bool` array are `true`. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) };
const a = array(Bool, [true, false, true]);
M.interact.reportBool(Array.or(a)); // true
```

### Array.product

This method returns the product of an array of unsigned integers:

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.product(a)); // 192
// or
M.interact.reportUInt(a.product()); // 192
```

### Array.reduce

Starting with the specified value, this method returns the [left fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) of the given function over the array:

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.reduce(a, 0, (x, total) => (total + x))); // 18
// or
M.interact.reportUInt(Array.reduce(a, 0, add)); // 18
// or
M.interact.reportUInt(a.reduce(0, (x, total) => (total + x))); // 18
// or
M.interact.reportUInt(a.reduce(0, add)); // 18
// also
M.interact.reportUInt(Array.iota(3).reduce(a, 0, (x, y, total) => (total + x + y))); // 21
```

### Array.reduceWithIndex

This method is similar to `Array.reduce` except that it provides the function with the element's index, too.

### Array.replicate

This method creates an array of the specified length where each element is the specified value:

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 3)], Null) };
M.interact.reportUIntArray(Array.replicate(3, 256)); // array(UInt, [256, 256, 256])
```

### Array.set

This method returns an array identical to the specified array except that the specified index contains the specified value.

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 3)], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUIntArray(Array.set(a, 1, 256)); // array(UInt, [4, 256, 8])
// or
M.interact.reportUIntArray(a.set(1, 256)); // array(UInt, [4, 256, 8])
```

### Array.size

This method returns the number of elements in the array.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.size(a)); // 3
// or
M.interact.reportUInt(a.size()); // 3
```

### Array.slice

This method creates a new array which is that portion of the given array defined by a starting index and a length.

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 3)], Null) };
const a = array(UInt, [4, 6, 8, 10, 12, 14]);
M.interact.reportUIntArray(Array.slice(a, 1, 3)); // array(UInt, [6, 8, 10])
// or
M.interact.reportUIntArray(a.slice(1, 3)); // array(UInt, [6, 8, 10])
```

### Array.sum

This method returns the sum of an array of unsigned integers.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.sum(a)); // 18
// or
M.interact.reportUInt(a.sum()); // 18
```

### Array.withIndex

This method creates a new same-length array where each element pairs the corresponding original element with its index.

``` js nonum
// index.rsh
const myInteract = { reportTupleArray: Fun([Array(Tuple(UInt, UInt), 3)], Null) };
const a = array(UInt, [4, 6, 8]);
M.interact.reportTupleArray(Array.withIndex(a)); // array(Tuple(UInt, UInt), [[4, 0], [6, 1], [8, 2]])
// or
M.interact.reportTupleArray(a.withIndex()); // array(Tuple(UInt, UInt), [[4, 0], [6, 1], [8, 2]])
```

### Array.zip

Given two same-length arrays, this method creates a new same-length array where each element pairs the elements of the original arrays.

``` js nonum
// index.rsh
const myInteract = { reportTupleArray: Fun([Array(Tuple(UInt, UInt), 3)], Null) };
const a = array(UInt, [4, 6, 8]);
const b = array(UInt, [10, 12, 14]);
M.interact.reportTupleArray(Array.zip(a, b)); // array(Tuple(UInt, UInt), [[4, 10], [6, 12], [8, 14]])
// or
M.interact.reportTupleArray(a.zip(b)); // array(Tuple(UInt, UInt), [[4, 10], [6, 12], [8, 14]])
```

# Bool

A `Bool` is a boolean which may be `true` or `false`.

# Bytes

`Bytes` is a specialized (immutable, fixed-length) array of characters:

``` js nonum
// index.rsh
const ar = 'فراشة';          // Bytes(10)
const de = 'Schmetterling';  // Bytes(13)
const en = 'butterfly';      // Bytes(9)
const fr = 'papillon';       // Bytes(8)
const ga = 'féileacán';      // Bytes(11)
const he = 'פַּרְפַּר';           // Bytes(18)
const vi = 'Con bướm';       // Bytes(11)
const zh = '蝴蝶';            // Bytes(6)
```

Two `Bytes` of different lengths are not interchangeable. Consider the following:

``` js nonum
// index.rsh
const myInteract = { reportBytes: Fun([Bytes(24)], Null) };
M.interact.reportBytes('butterfly'); // Bytes(9)
```

Because `'butterfly'` is of type `Bytes(9)` and `reportBytes` requires an argument of type `Bytes(24)`, the compiler, encountering this code, will generate an error similar to the following:

``` nonum
reachc: error[RE0088]: These types are mismatched: Bytes(9) vs Bytes(24)
```

To match the two types, use the `pad` function:

``` js nonum
// index.rsh
const myInteract = { reportBytes: Fun([Bytes(24)], Null) };
M.interact.reportBytes(Bytes(24).pad('butterfly'));
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportBytes: (v) => { console.log(`Length is ${v.length}. Value is ${v}.`); }};
```

Note that the lengths vary:

``` nonum
Length is 19. Value is فراشة.
Length is 24. Value is Schmetterling.
Length is 24. Value is butterfly.
Length is 24. Value is papillon.
Length is 22. Value is féileacán.
Length is 15. Value is פַּרְפַּר.
Length is 21. Value is Con bướm.
Length is 20. Value is 蝴蝶.
```

Another frontend `reportBytes` implementation enables you to inspect the strings as byte arrays:

``` js nonum
// index.mjs
const myInteract = {
  reportBytes: (v) => { 
    var myBuffer = [];
    var buffer = new Buffer(v, 'utf16le');
    for (var i = 0; i < buffer.length; i++) {
        myBuffer.push(buffer[i]);
    }
    console.log(...myBuffer);
  },
};
```

Here is the output:

``` nonum
65 6 49 6 39 6 52 6 41 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
83 0 99 0 104 0 109 0 101 0 116 0 116 0 101 0 114 0 108 0 105 0 110 0 103 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
98 0 117 0 116 0 116 0 101 0 114 0 102 0 108 0 121 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
112 0 97 0 112 0 105 0 108 0 108 0 111 0 110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
102 0 233 0 105 0 108 0 101 0 97 0 99 0 225 0 110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
228 5 188 5 183 5 232 5 176 5 228 5 188 5 183 5 232 5 0 0 0 0 0 0 0 0 0 0 0 0
67 0 111 0 110 0 32 0 98 0 176 1 219 30 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
116 135 118 135 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

# Contract

`Contract` is *not* an explicit type in *index.rsh* files. However, the functions `balance`, `getAddress`, `getContract`, and `transfer` implicitly refer to the contract:

### balance

The `balance` function returns the balance of the contract account.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) };
M.interact.reportUInt(balance());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportUInt: (v) => { console.log(`${v}`); } };
```

### getAddress

The `getAddress` function returns the `Address` of the contract:

``` js nonum
// index.rsh
const myInteract = { reportAddress: Fun([Address], Null) };
M.interact.reportAddress(getAddress());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportAddress: (v) => { console.log(v); } };
```

And, here is the output:

``` nonum
# REACH_CONNECTOR_MODE=ALGO-devnet
0x5b26ec467ab3e54c793315b548923ec376624d3d29e70083bd2555ef24da8383
  
# REACH_CONNECTOR_MODE=CFX-devnet
NET999:ACD8UE5KYBVG62MB9Z8R20CFBPHJ632ESJTEKUZ31V
  
# REACH_CONNECTOR_MODE=ETH-devnet
0xD2651496526c4E93fc01A87F99bD38092A244FB5
```

### getContract

The `getContract` function returns the `Contract` information representing the contract:

``` js nonum
// index.rsh
const myInteract = { reportContract: Fun([Contract], Null) };
M.interact.reportContract(getContract());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportContract: (v) => { console.log(v); } };
```

And, here is the output:

``` nonum
# REACH_CONNECTOR_MODE=ALGO-devnet
8
  
# REACH_CONNECTOR_MODE=CFX-devnet
NET999:ACC3MBPMGWWKET1Z405M7CT697775FMKZPBURP3CVZ
  
# REACH_CONNECTOR_MODE=ETH-devnet
0x6Da448EeE4A2B10aaE01307C81247cb39A4fea89
```

### transfer

A `transfer` expression transfers the specified number of tokens from the contract account to the specified participant account. Here is the declaration:

``` js nonum
transfer(AMOUNT_EXPR).to(ADDR_EXPR)
// or
transfer(AMOUNT_EXPR, TOKEN_EXPR).to(ADDR_EXPR)
```

* `AMOUNT_EXPR` is an unsigned integer specifying the number of tokens to transfer from the contract. This amount must evaluate to less than or equal to the balance of network tokens in the contract account.
* `ADDR_EXPR` is the target address for the transfer.
* `TOKEN_EXPR` is a Token type. This argument is optional.

Here is an example:

``` js nonum
transfer(10).to(Alice);
```

# Data

Use `Data` to define a data type (e.g. `Shape`):

``` js nonum
const Shape = Data({
  Circle: Object({ radius: UInt }),
  Square: Object({ side: UInt }),
  Rect: Object({ width: UInt, height: UInt })
});
```

In the example, the `Shape` data type has three variants: `Circle`, `Square`, and `Rect`. Create data instances like this:

``` js nonum
const circle = Shape.Circle({ radius: 7 });
const square = Shape.Square({ side: 5 });
const rect = Shape.Rect({ width: 3, height: 6 });
```

Data instances (e.g. `circle`, `square`, `rect`) are consumed by `switch` statements and `match` expressions.

# Digest

A `Digest` is a cryptographic hash of a value:

``` js 
// index.rsh
const sellerInteract = {
  password: Bytes(24), // "sellerPwd123$"
  reportDigest: Fun([Digest], Null)
};

export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', {});
  deploy();
  S.publish();
  commit();
  B.publish();
  commit();

  S.only(() => {
    const _pwd = digest(interact.password);
    interact.reportDigest(_pwd);
    const pwdDigest = declassify(_pwd);
  });
  S.publish(pwdDigest);
  commit();

  exit();
});
```

* Line 3: Assume that the frontend furnishes "sellerPwd123$" as the clear-text password.
* Line 17: `digest` converts the password to a hash (`0x51db28...`).
* Line 18: `reportDigest` passes the hash value to the frontend to demonstrate the use of the `Digest` type and to show you one way to inspect the hash value. Normally, you would not pass a hash value to a frontend.

The hash value for "sellerPwd123$" is the following:

``` nonum
0x51db281cfa433bd72491c79c46d0a2d1da211fda094e5904f2d3d77bbd51a2d5
```

You cannot *un*-digest a hash value to reveal the original string, but, you can compare hashed values:

``` js nonum
require( pwdDigest == digest(buyerPassword) );
```

# Either

`Either` is a built-in [Data](#data) type with two variants (i.e. `Left` and `Right`). It is implemented as a function that assigns type `A` to `Left` and type `B` to `Right`:

``` js nonum
export const Either = (A, B) => Data({Left: A, Right: B});
```

`Either` is useful for representing either the successful (i.e. `Right`) or the unsuccessful (i.e. `Left`) state of a return value. Consider, for example, a function called `addEvenNumbers` which only adds even numbers, and returns a value of type `Either` containing a sum in the `Right` variant if both arguments are even, and an object (containing the first offending argument and a message) in the `Left` variant if either argument is odd:

``` js nonum
// index.rsh
function addEvenNumbers(a, b) {
  const answer = Either(Object({ arg: UInt, msg: Bytes(40) }), UInt);
  if(a % 2 != 0) {
    return answer.Left({arg: a, msg: Bytes(40).pad('First arg must be an even number.')});
  } else if (b % 2 != 0) {
    return answer.Left({arg: b, msg: Bytes(40).pad('Second arg must be an even number.')});
  }
  return answer.Right(a + b);
}
```

One way to invoke `addEvenNumbers` and deal with the return value is like this:

``` js nonum
// index.rsh
const myInteract = {
  reportLeft: Fun([Object({"arg": UInt, "msg": Bytes(40)})], Null),
  reportRight: Fun([UInt], Null)
};

const myEither = addEvenNumbers(6, 4);
M.only(() => { 
  either(myEither, interact.reportLeft, interact.reportRight);
}
```

Reach provides the following methods for processing `Either` types:

### either

This method determines whether an argument of type `Either` is a `Left` or `Right` variant, and then invokes the relevant function passing the relevant variant as an argument:

``` js nonum
// index.rsh
const myInteract = {
  reportLeft: Fun([Object({"arg": UInt, "msg": Bytes(40)})], Null),
  reportRight: Fun([UInt], Null)
};

const myEither = addEvenNumbers(6, 4);
M.only(() => { 
  either(myEither, interact.reportLeft, interact.reportRight);
}
```

### fromLeft

This method returns the `Left` variant or, if the `Left` variant does not exist, a default value:

``` js nonum
// index.rsh
const myInteract = {
  reportLeft: Fun([Object({"arg": UInt, "msg": Bytes(40)})], Null)
};

const myEither = addEvenNumbers(5, 4);
S.only(() => { 
  interact.reportLeft(fromLeft(myEither, { arg: 0, msg: Bytes(40).pad('No error.')}));
});
```

### fromRight

This method returns the `Right` variant or, if the `Right` variant does not exist, a default value:

``` js nonum
// index.rsh
const myInteract = {
  reportRight: Fun([UInt], Null)
};

const myEither = addEvenNumbers(5, 4);
S.only(() => { 
  interact.reportRight(fromRight(myEither, 0));
});
```

### isLeft

This method returns `true` if the `Left` variant exists:

``` js nonum
// index.rsh
const myInteract = {
  reportBool: Fun([Bool], Null)
};

const myEither = addEvenNumbers(5, 4);
S.only(() => { 
  interact.reportBool(isLeft(myEither));
});
```

### isRight

This method returns `true` if the `Right` variant exists:

``` js nonum
// index.rsh
const myInteract = {
  reportBool: Fun([Bool], Null)
};

const myEither = addEvenNumbers(5, 4);
S.only(() => { 
  interact.reportBool(isRight(myEither));
});
```

### left

This method assigns a value to the `Left` variant:

``` js nonum
// index.rsh
const answer = Either(Object({ arg: UInt, msg: Bytes(40) }), UInt);
answer.Left({arg: a, msg: Bytes(40).pad('First arg must be an even number.')});
```

### right

This method assigns a value to the `Right` variant:

``` js nonum
// index.rsh
const answer = Either(Object({ arg: UInt, msg: Bytes(40) }), UInt);
answer.Right(a + b);
```

# FixedPoint

A `FixedPoint` is an object `{ sign: Bool, i: { scale: UInt, i: UInt} }` where `scale` is `1`, `10`, `100`, `1000`, etc., and `i` is the underlying unsigned integer. The value of a fixed-point number is `(i / scale) * sign`. It is used to represent numbers that have a fixed number of digits after the decimal.

# Foldable

`Foldable` acts as a base class to `Array` and `Map` providing the methods listed below. See [Array](#array) and [Map](#map) for details.

``` nonum
Foldable.all
Foldable.and
Foldable.any
Foldable.average
Foldable.count
Foldable.forEach
Foldable.includes
Foldable.max
Foldable.min
Foldable.or
Foldable.product
Foldable.size
Foldable.sum
```

# Fun

A `Fun` type declares a function in a participant interact interface. 

### Fun Example 1

The following example declares `reportUInt`:

``` js nonum
// index.rsh
const myInteract = {
  reportUInt: Fun([UInt], Null)
};
```

The first argument (e.g. `[UInt]`) is the array of arguments (by type) accepted by the function. `reportUInt` accepts one argument of type `UInt`. The second argument is the type of value returned by the function. `reportUInt` returns `Null`. 

### Fun Example 2

This example declares `willPurchase`:

``` js nonum
// index.rsh
const myInteract = {
  willPurchase: Fun( [Bytes(24), UInt, UInt], Bool)
};
```

`willPurchase` accepts three arguments. `Bytes[(24)` is a token name. The first `UInt` is the price. The second `UInt` is the quantity. `willPurchase` returns a `Bool`, `true` for *purchase* and `false` for *do not purchase*. 

### Fun Example 3

This example declares `shop`:

``` js nonum
// index.rsh
const product = Object({ name: Bytes(24), price: UInt })
const myInteract = {
  shop: Fun( [Array(product, 3)], UInt) ),
};
```

`shop` accepts one argument which is an `Array` of project objects where each object has a name and a price. `shop` returns a `UInt` which is a zero-based index (e.g. 0, 1, or 2) into the array. 

# Int

An `Int` is an object `{ sign: Bool, i: UInt }` rather than a scalar value because some consensus networks do not support signed integers.

### iadd

``` js nonum
iadd(a, b) // iadd(Int, Int)
```

### isub

``` js nonum
isub(a, b) // isub(Int, Int)
```

### imul

``` js nonum
imul(a, b) // imul(Int, Int)
```

### idiv

``` js nonum
idiv(a, b) // idiv(Int, Int)
```

### imod

``` js nonum
imod(a, b) // imod(Int, Int)
```

# Interval

Constructors

Accessors

Relational Operations

Arithmetic Operations

Other Operations

# Map

A `Map` is a means of associating participants with values of a given type:

|Participant|Value|
|-|-|
|Participant 1|`200`|
|Participant 2|`400`|
|Participant 3|`100`|
|Participant 4|`125`|

A `Map` represents participants by account address:

|Participant|Value|
|-|-|
|`0x1234abcd0`|`200`|
|`0x1234abcd1`|`400`|
|`0x1234abcd2`|`100`|
|`0x1234abcd3`|`125`|

The `Map` constructor requires a type:

``` js nonum
const m = new Map(UInt);
```

Use `[]` to reference (Line 10) and dereference (Line 11):

``` js
// index.rsh
const sellerInteract = {
  reportData: Fun([Data({"None": Null, "Some": UInt})], Null),
};
export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  deploy();
  // ...
  const m = new Map(UInt); // Address <> UInt
  m[S] = 256;              // reference
  const v = m[S];          // dereference
  S.interact.reportData(v);
  // ...
});
```

In the example above, `const v` is not a `UInt`. It is a `Data({"None": Null, "Some": UInt})` because `m[S]` allows for the possibility that the specified participant address is not in the `Map`. See [Maybe](#maybe).

`Map` is a `Foldable` container, so it includes all the [Foldable](#foldable) methods, but these methods are only valid within the `invariant` of a `while` loop. Consider the following:

``` js
const ctMap = new Map(UInt);
// ...
.invariant( balance() == sum && balance() == ctMap.sum() )
.while( !stop && balance() < p.goal )
// ...
```

Line 3 shows `Map.sum` within the loop invariant. See [Loop Invariant](/en/essentials/terminology/#loop-invariant) for a more detailed example.

### Map.all

This method returns `true` if every element in the map satisfies the given `Bool` function. Otherwise, it returns `false`. 

``` js nonum
// index.rsh
const m = new Map(UInt);
var x = 0;
invariant(balance() == 0 && Map.all(m, e => e < 5));
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.all(e => e < 5));
```

### Map.and

This method returns `true` if all elements in a map are `true`. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const m = new Map(Bool);
var x = 0;
invariant(balance() == 0 && Map.and(m));
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.and());
```

### Map.any

This method returns `true` if at least one element in the map satisfies the given `Bool` function. Otherwise, it returns `false`. 

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 0;
var x = 0;
invariant(balance() == 0 && Map.any(m, e => e < 5));
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.any(e => e < 5));
```

### Map.count

This method returns the number of elements in the map that satisfy the given `Bool` function.

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && Map.count(m, e => e == 5) == 1);
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.count(e => e == 5) == 1);
```

### Map.includes

This method returns `true` if the map contains the specified element. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && Map.includes(m, 5));
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.includes(5));
```

### Map.max

This method returns the largest number in a map of unsigned integers:

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && Map.max(m) == 5);
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.max() == 5);
```

### Map.min

This method returns the smallest number in a map of unsigned integers:

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && Map.min(m) == 5);
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.min() == 5);
```

### Map.or

This method returns `true` if any elements in a map are `true`. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const m = new Map(Bool);
m[S] = true;
var x = 0;
invariant(balance() == 0 && Map.or(m));
while ( x < 5 ) {
  // ...
}
// or 
invariant(balance() == 0 && m.or()); // Didn't work for MJH.
```

### Map.reduce

Starting with the specified value, this method returns the [left fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) of the given function over the map:

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && Map.reduce(m, 0, (e, total) => (total + e)) > 0);
while ( x < 5 ) {
  // ...
}
// or
invariant(balance() == 0 && Map.reduce(m, 0, add) > 0);
// or
invariant(balance() == 0 && m.reduce(0, (e, total) => (total + e)) > 0);
// or
invariant(balance() == 0 && m.reduce(0, add) > 0);
```

### Map.size

This method returns the number of elements in the map.

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && m.size() == 1);
while ( x < 5 ) {
  // ...
}
```

### Map.sum

This method returns the sum of a map of unsigned integers.

``` js nonum
// index.rsh
const m = new Map(UInt);
m[S] = 5;
var x = 0;
invariant(balance() == 0 && Map.sum(m) > 0);
while ( x < 5 ) {
  // ...
}
// or
invariant(balance() == 0 && m.sum() > 0);
```

# Maybe

`Maybe` is a built-in [Data](#data) type with two variants (i.e. `None` and `Some`). It is implemented as a function that assigns a particular type `A` to the `Some` variant:

``` js nonum
export const Maybe = (A) => Data({ None: Null, Some: A });
```

`Maybe` is useful when a particular action may or may not return a valid value. Dereferencing a [Map](#map) is one example:

``` js nonum
const myMap = new Map(UInt);
// ...
const myMaybe = myMap[Seller]; // Maybe
```

In the example above, `Seller` may or may not be a valid address in `myMap`, so `myMap[Seller]` returns a `Maybe` to cover both possibilities. Programs use the functions `fromSome`, `isNone`, and `isSome` to evaluate `Maybe` values as described below.

Programs create `Maybe` values like this:

``` js nonum
const mUInt = Maybe(UInt);
const success = mUInt.Some(256);
const failure = mUInt.None();
```

### fromSome

This method takes a `Maybe` value and a default value as arguments. It returns the `Some` variant is it exists. Otherwise, it returns the default value:

``` js nonum
const v = fromSome(myMaybe, 0);
if(v) {
  // ...
} else {
  // ...
}
```

### isNone

This method returns `true` if the specified `Maybe` argument is `None`:

``` js nonum
if( isNone( myMaybe ) ) {
  // ...
}
```

### isSome

This method returns `true` if the specified `Maybe` argument is `Some`:

``` js nonum
if( isSome( myMaybe ) ) {
  // ...
}
```

# Null

`Null` or `null` is the intentional absence of any value.

# Object

An `Object` is a container of key-value pairs where keys are identifiers (e.g. `name`) or strings (e.g. `'name'`), and values are expressions:

``` js nonum
// index.rsh
const product = {
  name: Bytes(24).pad('Carrots'),
  unit: Bytes(10).pad('Bunch'),
  units: Bytes(10).pad('Bunches'),
  price: 100
};
```

A Reach backend might pass `product` to a frontend like this:

``` js nonum
const myInteract = {
  reportObject: Fun([Object({name: Bytes(24), price: UInt, unit: Bytes(10), units: Bytes(10)})], Null)
};
M.interact.reportObject(product);
```

Objects have the following methods:

### Object.has

This method returns `true` if the specified object has the specified field. Otherwise, it returns `false`:

``` js nonum
const myInteract = { reportBool: Fun([Bool], Null) };
const product = {
  name: Bytes(24).pad('Carrots'),
  unit: Bytes(10).pad('Bunch'),
  units: Bytes(10).pad('Bunches'),
  price: 100
};
M.interact.reportBool(Object.has(product, 'name')); // true
```

### Object.set

This method returns a new object identical to the specified one except that the specified field (which may or may not exist in the original object) is set to the specified value. The following example resets an existing field:

``` js nonum
const myInteract = {
  reportObject: Fun([Object({name: Bytes(24), price: UInt, unit: Bytes(10), units: Bytes(10)})], Null)
};
const product = {
  name: Bytes(24).pad('Carrots'),
  unit: Bytes(10).pad('Bunch'),
  units: Bytes(10).pad('Bunches'),
  price: 100
};
const product2 = Object.set(product, "price", 200);
M.interact.reportObject(product2);
```

The following example adds a new field:

``` js nonum
const sellerInteract = {
  reportObject: Fun([Object({name: Bytes(24), price: UInt, unit: Bytes(10), units: Bytes(10), color: Bytes(12)})], Null)
};
const product = {
  name: Bytes(24).pad('Carrots'),
  unit: Bytes(10).pad('Bunch'),
  units: Bytes(10).pad('Bunches'),
  price: 100
};
const product2 = Object.set(product, "color", Bytes(12).pad("orange"));
S.interact.reportObject(product2);
```

### Object.setIfUnset

This method is similar to [Object.set](#objectset). However, `Object.setIfUnset` will not overwrite an existing property value. It will only add a new property and value to the specified object.

# Participant

A `Participant` expression defines a participant (Lines 12 and 13):

``` js
// index.rsh
const sellerInteract = {
  price: UInt,
  reportReady: Fun([UInt], Null)
};

const buyerInteract = {
  confirmPurchase: Fun([UInt], Bool),
};

export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', buyerInteract);
  deploy();
  // ...
});
```

A `Participant`, which represents a real-world entity such as a human being, *participates* in transactions within the smart contract. A participant has a name and an interact object. The Reach compiler uses the participant name (e.g. *Seller*) in the *index.rsh* file to name the corresponding function in *index.main.mjs* which, among other things, binds a participant to the consensus-network account of the caller:

``` js nonum
export async function Seller(ctc, interact) { /*...*/ }
```

The Participant uses the interact object to communicate with the Reach DApp frontend:

``` js nonum
S.interact.reportReady(price);
```

# ParticipantClass

A `ParticipantClass` expression defines a category of participants.

``` js nonum
// index.rsh
const hostApi = {};
const playerApi = {};

export const main = Reach.App(() => {
  const F = Participant('Host', hostApi);
  const S = ParticipantClass('Player', playerApi);
  deploy();
  // ...
});
```

The DApp above, for example, supports one host and many players. 

# Refine

# remote

# Set

A `Set` expression creates a specialized Array. The length of the array is the number of participants in the contract. The keys are the participant addresses. All values are boolean (initially set to `false`).

``` js nonum
const bidders = new Set();
bidders.insert(Alice);
bidders.remove(Alice);
bidders.member(Alice); // false
```

# Struct

A *struct* is ...

# Token

The `Token` expression mints a new non-network token. Here is the declaration:

``` js nonum
new Token(PARAMS)
```

* `PARAMS` is an object with the keys in the table below.

    |Key|Type|Default|
    |-|-|-|
    |`name`|`Bytes(32)`|empty|
    |`symbol`|`Bytes(8)`|empty|
    |`url`|`Bytes(96)`|empty|
    |`metadata`|`Bytes(32)`|empty|
    |`supply`|`UInt`|`UInt.max`|
    |`decimals`|`UInt`|ALGO = 6; CFX and ETH = 18|

This returns a Token value and deposits a supply amount of the new non-network tokens into the contract account associated with the DApp. These tokens must be destroyed by the end of the DApp. A token has the following methods:

* `Token.burn(tok, amt)` or `tok.burn(amt)`, where tok is a Token value and amt is a UInt value, may be used to burn tokens in the contract account, meaning that they are utterly destroyed and can never be recovered.

* `Token.destroy(tok)` or `tok.destroy()`, where tok is a Token value, may be used to destroy the token so that it may never be used again by any users on the consensus network. This must be called before the application exits.

* `Token.destroyed(tok)` or `tok.destroyed()`, where tok is a Token value, returns whether destroy has been called on tok yet.

* `Token.supply(tok)` or `tok.supply()`, where tok is a Token value, may be used to query the current supply of tokens, i.e. the number of tokens which have not been burnt.

Here is an example:

``` js nonum
require(supply >= 2 * amt);
const tok = new Token({ name, symbol, url, metadata, supply, decimals });
transfer(amt, tok).to(who);
tok.burn(amt);
assert(tok.supply() == supply - amt);
tok.burn();
assert(tok.destroyed() == false);
tok.destroy();
```

# Tuple

A *Tuple* is an immutable, ordered list of values. The size of a tuple is fixed. The values may be different types. Values may be referenced using a zero-based index. The following creates a tuple of type `Tuple(UInt, UInt, UInt)`:

``` js nonum
const t = [5, 10, 15];
const v = t[2]; // 15
```

The following creates a tuple of type `Tuple(UInt, Object({price: UInt, quantity: UInt}), UInt)`:

``` js nonum
const t = [5, { price: 1000, quantity: 40 }, 15];
const p = t[1].price; // 1000
const v = t[2];       // 15
```

The following creates a tuple of type `Tuple(UInt, UInt, UInt, UInt, Object({price: UInt, quantity: UInt}), UInt)`:

``` js nonum
const t_temp = [5, { price: 1000, quantity: 40 }, 15];
const t = [5, 10, 15, ...t_temp];
const p = t[4].price; // 1000
const v = t[5];       // 15
```

### Tuple.length

`Tuple.length` returns the length of the tuple:

``` js nonum
const lenA = Tuple.length([5, 10, 15]); // 3
// or
const lenB = [5, 10, 15].length; // 3
```

### Tuple.set

`Tuple.set` returns a new tuple identical to the original except that the value at the specified index is the new value:

``` js nonum
const tA = Tuple.set([5, 10, 15], 1, 44);
const vA = tA[1]; // 44
// or
const tB = [5, 10, 15].set(1, 44);
const vB = tB[1]; // 44
```

# UInt

### add

``` js nonum
a + b      // UInt + UInt
add(a, b)  // add(UInt, UInt)
```

### sub

``` js nonum
a - b      // UInt - UInt
sub(a, b)  // sub(UInt, UInt)
```

### mul

``` js nonum
a * b      // UInt * UInt
mul(a, b)  // mul(UInt, UInt)
```

### muldiv

The `muldiv` function performs `(a * b) / c`:

``` js nonum
muldiv(a, b, c) // muldiv(UInt, UInt, UInt)
```

The product of `a * b` may exceed `UInt.max`, but the final quotient must be less than `UInt.max`.

### div

``` js nonum
a / b      // UInt * UInt
div(a, b)  // div(UInt, UInt)
```

### mod

``` js nonum
a % b      // UInt % UInt
mod(a, b)  // mod(UInt, UInt)
```

### sqrt

The `sqrt` function finds the square root of `value`, iterating `iterations` number of times.

``` js nonum
sqrt(value, iterations) // sqrt(UInt, UInt)
```

This function utilizes the [Babylonian Method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method) for computing the square root. `iterations` must be known at compile time. When performing 5 iterations, the algorithm can reliably calculate the square root up to 32 squared, or 1,024. When performing 10 iterations, the algorithm can reliably calculate the square root up to 580 squared, or 336,400.

### pow

The `pow` function calculates the approximate value of raising `base` to `power`, iterating `iterations` number of times.:

``` js nonum
pow(base, power, iterations) // pow(UInt, UInt, UInt)
```

`iterations` must be known at compile time. Six iterations provides enough accuracy to calculate up to <code>2<sup>63</sup></code>.

# View

A View expression defines a view object that allows non-participants to see public variables in the contract. The following program instantiates the view in Line 4 and initializes the `price` property in Line 10:

``` js
// index.rsh
export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', buyerInteract);
  const V = View('Main', { price: UInt });
  deploy();

  S.only(() => { const price = declassify(interact.price); });
  S.publish(price);
  S.interact.reportReady(price);
  V.price.set(price);
  commit();
```

A frontend might access the view like this:

``` js nonum
const price = await ctc.views.Main.price();
console.log(`The price of wisdom is ${price[0] == 'None' ? '0' : toSU(price[1])} ${suStr}.`);
```
