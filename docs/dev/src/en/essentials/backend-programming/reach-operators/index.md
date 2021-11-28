---
menuItem: mi-docs
---

# Reach Operators

# Arithmetic Operators

These operators work on unsigned integers. Corresponding functions are `add`, `sub`, `mul`, `div`, and `mod`. 

| Operator | Name | Example |
|-|-|-|
|`+`| Add | `const v = 5 + 3; // 8`|
|`-`| Subtract | `const v = 5 - 3; // 2` |
|`*`| Multiply | `const v = 5 * 3; // 15` |
|`/`| Divide | `const v = 5 / 3; // 1` |
|`%`| Modulus | `const v = 5 % 3; // 2` |

# Assignment Operators

Reach supports only the assignment operator (i.e. `=`). All other operators (e.g. `+=`, `*=`, ...) are invalid because Reach does not support the reassignment of an identifier. 

| Operator | Name | Example |
|-|-|-|
|`=`|`Assign`| `const v = 5;` |

# Bitwise Operators

Bitwise operations are not supported by all consensus networks. They also decrease the efficiency of verification.

| Operator | Name | Example |
|-|-|-|
|`&`|`AND`| `const v = 15 & 29; // 13` |
|`\|`|`OR`| `const v = 12 \| 3; // 15` |
|`^`|`XOR`| `const v = 15 ^ 13; // 2` |
|`<<`|`Left Shift`| `const v = 2 << 3; // 16` |
|`>>`|`Right Shift`| `const v = 16 >> 3; // 2` |

# Comparison Operators

Comparison operators evaluate to `true` or `false`. Comparisons between values of different types are invalid. Therefore, `==` and `===` have the same meaning, and `!=` and `!==` do, too. 

| Operator | Name | Example |
|-|-|-|
|`==` <br/> `===`|`is equal to`| `x == y` <br/> `x === y`|
|`!=` <br/> `!==`|`is not equal to`| `x != y` <br/> `x !== y`|
|`>`|`is greater than`| `x > y`|
|`>=`|`is greater than or equal to`| `x >= y`|
|`<`|`is less than`| `x < y`|
|`<=`|`is less than or equal to`| `x <= y`|

# Logical Operators

Logical `and` evaluates to `true` when both the left-side expression and the right-side expression evaluate to `true`, and logical `or` evaluates to `true` when either the left-side expression or the right-side expression evaluates to `true`. 

| Operator | Name | Example |
|-|-|-|
|`&&`|and| `x && y`|
|`\|\|`|and| `x \|\| y`|

# Type Operator

Reach supports the `typeof` operator and two functions, `typeOf()` and `is()`, for examining data types of values.

| Operator | Name | Example |
|-|-|-|
|`typeof`|and| `typeof a`|
|`\|\|`|and| `x \|\| y`|

The `typeof` operator and the `typeOf` function return the data type of a value:

``` js nonum
const XTy = typeof 0;
const YTy = typeOf(true);
```

You can use the returned type in declarations like this:

``` js nonum
const myInteract = {
  getX: Fun([], XTy),
  getY: Fun([], YTy)
}
```

The `is` function verifies that a variable is of a certain type:

``` js nonum
const addOneImpl = (x) => x + 1;
export const addOne = is(addOneImpl, Fun([UInt], UInt));
```

Because `addOneImpl` is of type `Fun([UInt], UInt)`, `is()` returns `addOneImpl`. On the other hand, consider this:

``` js nonum
const addOneImpl = (x) => x + 1;
export const addOne = is(addOneImpl, Fun([UInt], Null)); // invalid
```

Because `addOneImpl` is not of type `Fun([UInt], Null)`, the compiler outputs an error:

``` nonum
These types are mismatched: UInt vs Null
```

# Ternary Operator

| Operator | Name | Example |
|-|-|-|
|`? :`|and| `a == b ? c : d`|
