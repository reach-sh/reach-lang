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
}
M.interact.reportAddress(this);
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = {
  reportAddress: (v) => { console.log(v); }
}
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
}
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

`API` arguments include a name and an interface comprised of methods available in frontends via the `ctc.apis` object. The return value is an object whose fields are the interface methods. The object may be used in the `.api` component of [fork](/en/essentials/backend-programming/reach-statements/#fork) and [parallelReduce](/en/essentials/backend-programming/reach-statements/#parallelreduce) expressions. Each object method must occur exactly once in the entire program. See [this example](https://github.com/reach-sh/reach-lang/blob/master/examples/api-full/index.rsh).

# Array

An *Array* is an immutable, ordered list of values. The size of an array is fixed, and the values must be of the same type. Values may be referenced using a zero-based index. The following creates an array of type `Array(UInt, 3)` from a tuple:

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
const myInteract = { reportBool: Fun([Bool], Null) }
const a = array(UInt, [4, 6, 8]);
M.interact.reportBool(Array.all(a, e => e > 0)); // true
// or
M.interact.reportBool(a.all(e => e > 0)); // true
```

### Array.and

This method returns `true` if all elements in a `Bool` array are `true`. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) }
const a = array(Bool, [true, false, true]);
M.interact.reportBool(Array.and(a)); // false
```

### Array.any

This method returns `true` if at least one element in the array satisfies the given `Bool` function. Otherwise, it returns `false`. 

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) }
const a = array(UInt, [4, 6, 8]);
M.interact.reportBool(Array.any(a, e => e > 7)); // true
// or
M.interact.reportBool(a.any(e => e > 7)); // true
```

### Array.average

This method returns the mean of an array of `UInts`.

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) }
const a = array(UInt, [4, 6, 8]);
M.interact.reportUInt(Array.average(a)); // 6
// or
M.interact.reportUInt(a.average()); // 6
```

### Array.concat

This method returns a new array representing the concatenation of the two given arrays.

``` js nonum
// index.rsh
const myInteract = { reportUIntArray: Fun([Array(UInt, 6)], Null) }
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
const myInteract = { reportUInt: Fun([UInt], Null) }
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

### Array.findIndex

### Array.forEach

### Array.includes

### Array.indexOf

### Array.iota

### Array.length

### Array.map

### Array.mapWithIndex

### Array.max

### Array.min

### Array.or

This method returns `true` if any elements in a `Bool` array are `true`. Otherwise, it returns `false`.

``` js nonum
// index.rsh
const myInteract = { reportBool: Fun([Bool], Null) }
const a = array(Bool, [true, false, true]);
M.interact.reportBool(Array.or(a)); // true
```

### Array.product

### Array.reduce

### Array.reduceWithIndex

### Array.replicate

### Array.set

### Array.size

### Array.slice

### Array.sum

### Array.withIndex

### Array.zip

# Bool

A *Bool* is a boolean which may be `true` or `false`.

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
const myInteract = { reportBytes: Fun([Bytes(24)], Null) }
M.interact.reportBytes('butterfly'); // Bytes(9)
```

Because `'butterfly'` is of type `Bytes(9)` and `reportBytes` requires an argument of type `Bytes(24)`, the compiler, encountering this code, will generate an error similar to the following:

``` nonum
reachc: error[RE0088]: These types are mismatched: Bytes(9) vs Bytes(24)
```

To match the two types, use the `pad` function:

``` js nonum
// index.rsh
const myInteract = { reportBytes: Fun([Bytes(24)], Null) }
M.interact.reportBytes(Bytes(24).pad('butterfly'));
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportBytes: (v) => { console.log(`Length is ${v.length}. Value is ${v}.`); }}
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

Another `reportBytes` implementation enables you to inspect the strings as byte arrays:

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
}
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

### balance

The `balance` function returns the balance of the contract account.

``` js nonum
// index.rsh
const myInteract = { reportUInt: Fun([UInt], Null) }
M.interact.reportUInt(balance());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportUInt: (v) => { console.log(`${v}`); } }
```

### getAddress

The `getAddress` function returns the `Address` of the contract:

``` js nonum
// index.rsh
const myInteract = { reportAddress: Fun([Address], Null) }
M.interact.reportAddress(getAddress());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportAddress: (v) => { console.log(v); } }
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
const myInteract = { reportContract: Fun([Contract], Null) }
M.interact.reportContract(getContract());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = { reportContract: (v) => { console.log(v); } }
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

# Data

A *data instance* is ...

# Digest

# Either

# FixedPoint

A `FixedPoint` is an object `{ sign: Bool, i: { scale: UInt, i: UInt} }` where `scale` is `1`, `10`, `100`, `1000`, etc., and `i` is the underlying unsigned integer. The value of a fixed-point number is `(i / scale) * sign`. It is used to represent numbers that have a fixed number of digits after the decimal.

# Fun

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

A *map* is ...

### Map.all

### Map.and

### Map.any

### Map.average

### Map.count

### Map.forEach

### Map.includes

### Map.max

### Map.min

### Map.or

### Map.product

### Map.reduce

### Map.size

### Map.sum

# Maybe

### Some

### None

# Null

`Null` or `null` is the intentional absence of any value.

# Object

An *object* is ...

### Object.has

### Object.set

### Object.setIfUnset

# Participant

# ParticipantClass

# Refine

# remote

# Set

# Struct

A *struct* is ...

# Token

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