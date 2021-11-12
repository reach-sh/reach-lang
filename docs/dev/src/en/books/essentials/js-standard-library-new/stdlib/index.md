---
menuItem: mi-docs
---

# Stdlib Object

# Properties

## atomicUnit

This string property represents the smallest indivisible unit of measure for the standard unit. For Algorand, the standard unit is ALGO and the atomic unit is μALGO. For Ethereum, the standard unit is ETH and the atomic unit is WEI.

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(stdlib.atomicUnit);
})();
```

### Output

```
μALGO  # ALGO
WEI    # ETH
```

## connector

This property contains a string representing the current consensus network.

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(stdlib.connector);
})();
```

### Output

```
ALGO
ETH
```

## hasRandom

This property is the following object: {random: [Function: randomUInt]}.

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  for (let i = 0; i < 5; i++) {
    console.log(`Random #${i+1}: ${stdlib.hasRandom.random()}`);
  }
})();
```

### Output

```
# ALGO
Random #1: 17830142387684920417
Random #2: 17746974019939158498
Random #3: 9160898625104005994
Random #4: 3613411261652842147
Random #5: 983361309112715662

# ETH
Random #1: 27607848804729973581014410502293711971761448598557376981283183717322704017433
Random #2: 60037153247786503166971761128506669526562161409390884377433244834184239865070
Random #3: 58769759163361652927045305807023328094637415231145708005951464780084755561144
Random #4: 35785320426544831645097143845375516087094351111486972004275998413013072818256
Random #5: 38153054402165990843880482716988449931280675794450268073745162749585985896245
```

## minimumBalance

This BigInteger property represents the minimum balance in atomic units for any account associated with the consensus network.

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(`Minimum balance is ${stdlib.minimumBalance}.`);
})();
```

### Output

```
Minimum balance is 100000. # ALGO
Minimum balance is 0.      # ETH
```

## standardUnit

This string property represents the network token unit most commonly associated with a network. For Algorand, the standard unit is ALGO. For Ethereum, the standard unit is ETH.

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(stdlib.standardUnit);
})();
```

### Output

```
ALGO
ETH
```

# Methods

## add

This method performs integer addition.

### Signature

```
add(x, y) => sum
x must be an integer.
y must be an integer.
sum is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const x = 1
  const y = 2
  const sum = stdlib.add(x, y)
  console.log(`${x} + ${y} = ${sum}`)
})();
```

### Output

```
1 + 2 = 3
```

## addressEq

This method compares network addresses for equality.

### Signature

```
addressEq(address1, address2) => tf
address1 is a string representing a network address.
address2 is a string representing a network address.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balanceIn = stdlib.parseCurrency(10);
  const account1 = await stdlib.newTestAccount(balanceIn);
  const account2 = await stdlib.newTestAccount(balanceIn);
  const address1 = account1.getAddress();
  const address2 = account2.getAddress();
  console.log(`Address 1: ${address1}`);
  console.log(address1 == address2 ? '==' : '!=');
  console.log(stdlib.addressEq(address1, address2) ? '==' : '!=');
  console.log(`Address 2: ${address2}`);
})();
```

### Output

```
Address 1: 0x4CCEa1a6782deB31Ee30C70F26F2c045bD7bc22e
!=
!=
Address 2: 0x71e337b01b5b9479f2D7993A286c177728f7E3eb
```

## argsSlice

This method creates an array containing the last count elements of the original array.

### Signature

```
argsSlice(arr1, count) => arr2
arr1 is an array.
count is an integer between 0 and arr1.length inclusive.
arr2 is a new array.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const arr = [1,2,3,4,5]
  console.log(stdlib.argsSlice(arr, 0))
  console.log(stdlib.argsSlice(arr, 1))
  console.log(stdlib.argsSlice(arr, 2))
  console.log(stdlib.argsSlice(arr, 3))
  console.log(stdlib.argsSlice(arr, 4))
  console.log(stdlib.argsSlice(arr, 5))
  console.log(stdlib.argsSlice(arr, 100))  // count > arr.length
  console.log(stdlib.argsSlice(arr, -100)) // count < arr.length
})();
```

### Output

```
[]
[ 5 ]
[ 4, 5 ]
[ 3, 4, 5 ]
[ 2, 3, 4, 5 ]
[ 1, 2, 3, 4, 5 ]
[ 1, 2, 3, 4, 5 ] # count > arr.length
[]                # count < arr.length
```

## argsSplit

This method creates an array containing two new arrays containing the elements of the original array split at count.

### Signature

```
argsSplit(arr1, count) => arr2
arr1 is an array.
count is an integer between 0 and arr1.length inclusive.
arr2 is a new array containing two new arrays.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const arr = [1,2,3,4,5]
  console.log(stdlib.argsSplit(arr, 0))
  console.log(stdlib.argsSplit(arr, 1))
  console.log(stdlib.argsSplit(arr, 2))
  console.log(stdlib.argsSplit(arr, 3))
  console.log(stdlib.argsSplit(arr, 4))
  console.log(stdlib.argsSplit(arr, 5))
  console.log(stdlib.argsSplit(arr, 100))  // count > arr.length
  console.log(stdlib.argsSplit(arr, -100)) // count < arr.length
})();
```

### Output

```
[ [ 1, 2, 3, 4, 5 ], [] ]
[ [ 1, 2, 3, 4 ], [ 5 ] ]
[ [ 1, 2, 3 ], [ 4, 5 ] ]
[ [ 1, 2 ], [ 3, 4, 5 ] ]
[ [ 1 ], [ 2, 3, 4, 5 ] ]
[ [], [ 1, 2, 3, 4, 5 ] ]
[ [], [ 1, 2, 3, 4, 5 ] ] # count > arr.length
[ [ 1, 2, 3, 4, 5 ], [] ] # count < arr.length
```

## Array_set

This method creates and returns a copy of an array, overwriting an original value with a new value at the specified position.

### Signature

```
Array_set(arr1, index, value) => Promise<arr2>
arr1 is the original array.
index is the 0-based index.
value is the replacement value.
arr2 is the new array.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const arr1 = [1,2,3,4,5]
  const arr2 = stdlib.Array_set(arr1, 1, 9)
  console.log(arr1)
  console.log(arr2)
})();
```

### Output

```
[ 1, 2, 3, 4, 5 ]
[ 1, 9, 3, 4, 5 ]
```

## Array_zip

This method creates an array of two-element arrays by combining two same-length arrays where arr1[0] and arr2[0] form the first two-element array, arr1[1] and arr2[1] for the second, etc. The elements of the new array are the actual elements of the parameters. They are not copies.

### Signature

```
Array_zip(arr1, arr2) => arr3
arr1 is an array.
arr2 is an array.
arr3 is an array.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();

  console.log('\n', 'Example 1:');
  const arr1 = ['one', 'two', 'three'];
  const arr2 = [1, 2, 3];
  const nums = stdlib.Array_zip(arr1, arr2)
  console.log(nums);

  console.log('\n', 'Example 2:');
  const p1 = {fname: 'Jane', lname: 'Doe', age: 30};
  const p2 = {fname: 'John', lname: 'Jie', age: 25};
  const p = [p1, p2]
  const a1 = {street: '123 Main Street', city: 'London'};
  const a2 = {street: '456 Pine Street', city: 'Munich'};
  const a = [a1, a2];
  const pa = stdlib.Array_zip(p, a)
  console.log(pa);

  console.log('\n', 'Example 3:');
  console.log(p1);
  console.log(`is${p1 === pa[0][0] ? '' : ' NOT'} the same instance as`);
  console.log(pa[0][0]);
})();
```

### Output

```
Example 1:
[ [ 'one', 1 ], [ 'two', 2 ], [ 'three', 3 ] ]

Example 2:
[
  [
    { fname: 'Jane', lname: 'Doe', age: 30 },
    { street: '123 Main Street', city: 'London' }
  ],
  [
    { fname: 'John', lname: 'Jie', age: 25 },
    { street: '456 Pine Street', city: 'Munich' }
  ]
]

Example 3:
{ fname: 'Jane', lname: 'Doe', age: 30 }
is the same instance as
{ fname: 'Jane', lname: 'Doe', age: 30 }
```

## assert

## balanceOf

This method returns the number of network tokens in atomic units held by the account.

### Signature

```
balanceOf(account) => Promise<amount>
account is a Reach account object.
amount is a BigNumber of tokens in atomic units.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balanceIn = stdlib.parseCurrency(10);
  const account = await stdlib.newTestAccount(balanceIn);
  const balanceOut = await stdlib.balanceOf(account);
  console.log(`Balance: ${balanceOut} ${stdlib.atomicUnit}.`);
})();
```

### Output

```
Balance: 10000000 μALGO.           # ALGO
Balance: 10000000000000000000 WEI. # ETH
```

## bigNumberify

This method converts a number into a BigNumber.

### Signature

```
bigNumberify(n) => bn
n is a number.
bn is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  [5, '5'].forEach(v => {
    const bn = stdlib.bigNumberify(v);
    console.log(`Converting the ${typeof v} ${v} to a ${bn.constructor.name} ${bn}.`);
  });
})();
```

### Output

```
Converting the number 5 to a BigNumber 5.
Converting the string 5 to a BigNumber 5.
```

## bigNumberToHex

This method converts a BigNumber into a hexadecimal string.

### Signature

```
bigNumberToHex(bn) => s
bn is a BigNumber.
s is a string.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const bn = stdlib.bigNumberify(5);
  const s = stdlib.bigNumberToHex(bn);
  console.log(`Type:  ${typeof s}`);
  console.log(`Value: 0x${s}`)
})();
```

### Output

```
Type:  string
Value: 0x0000000000000000000000000000000000000000000000000000000000000005
```

## bigNumberToNumber

This method converts a BigNumber into a number.

### Signature

```
bigNumberToNumber(bn) => n
bn is a BigNumber.
n is a number.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const bn = stdlib.bigNumberify(12345);
  const n = stdlib.bigNumberToNumber(bn);
  console.log(`Type:  ${typeof n}`);
  console.log(`Value: ${n}`)
})();
```

### Output

```
Type:  number
Value: 12345
```

## bytesEq

This method compares strings for equality.

### Signature

```
bytesEq(s1, s2) => tf
s1 is a string.
s2 is a string.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const s1 = 'test1';
  const s2 = 'test2';
  console.log(`String 1: ${s1}`);
  console.log(s1 == s2 ? '==' : '!=');
  console.log(stdlib.bytesEq(s1, s2) ? '==' : '!=');
  console.log(`String 2: ${s2}`);
})();
```

### Output

```
String 1: test1
!=
!=
String 2: test2
```

## checkedBigNumberify

## connectAccount

This method returns the Reach account object associated with the NetworkAccount object provided. The term connect in the method name does not refer to making a connection. Rather, it emphasizes that the returned Reach account object corresponds to the particular connected consensus network.

### Signature

```
connectAccount(networkAccount) => Promise<account>
networkAccount is consensus network account specification object.
account is a Reach account object.
```

### Example

```
This example program is somewhat contrived. It gets networkAccount from account, and then it gets the same account from networkAccount.

import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balance = stdlib.parseCurrency(10);
  const account = await stdlib.newTestAccount(balance);
  const networkAccount = account.networkAccount;
  const sameAccount = await stdlib.connectAccount(networkAccount)
  console.log(sameAccount);
})();
```

## createAccount

This method creates and returns a Reach account object with a balance of zero network tokens.

### Signature

```
createAccount() => Promise<account>
account is a Reach account object.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const account = await stdlib.createAccount();
  console.log(account);
})();
```

## debug

## digest

## digestEq

## div

This method performs integer division.

### Signature

```
div(x, y) => quot
x must be an integer.
y must be an integer.
quot is a BigNumber with no remainder.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const x = 20
  const y = 7
  const quot = stdlib.div(x, y)
  console.log(`${x} / ${y} = ${quot}`)
})();
```

### Output

```
20 / 7 = 2
```

## envDefault

I ran the following:

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(stdlib.envDefault(process.env));
  console.log(process.env);
})();
```

The only difference between the two is the following:

```
<   HOSTNAME: 'b43bddc766a8',
---
>   HOSTNAME: 'faaf8df0dc92',
```

## eq

This method determines if one integer is equal to another integer.

### Signature

```
eq(x, y) => tf
x is an integer.
y is an integer.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  let x = 4;
  [-4, 4, 8].forEach((y) => {
    console.log(`${x} == ${y} is ${stdlib.eq(x, y)} (${x == y})`)
  });
})();
```

### Output

```
4 == -4 is false (false)
4 == 4 is true (true)
4 == 8 is false (false)
```

## formatCurrency

This method converts a network token amount from an atomic unit number to a standard unit string.

### Signature

```
formatCurrency(auAmount, decimals) => suAmountStr
auAmount is a number of tokens in atomic units.
decimals is the number of decimal places to retain.
suAmountStr is a string representing tokens in standard units.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const suAmount = 100.5678;
  const auAmount = stdlib.parseCurrency(suAmount);
  const suAmountStr = stdlib.formatCurrency(auAmount, 2);
  console.log(`Standard Unit Amt = ${suAmount} ${stdlib.standardUnit}`)
  console.log(`Atomic Unit Amt   = ${auAmount} ${stdlib.atomicUnit}`)
  console.log(`Standard Unit Amt Str = ${suAmountStr} ${stdlib.standardUnit}`)
})();
```

### Output

```
# ALGO
Standard Unit Amt = 100.5678 ALGO
Atomic Unit Amt   = 100567800 μALGO
Standard Unit Amt Str = 100.56 ALGO

# ETH
Standard Unit Amt = 100.5678 ETH
Atomic Unit Amt   = 100567800000000000000 WEI
Standard Unit Amt Str = 100.56 ETH
```

## fundFromFaucet

This method adds the given balance of network tokens in atomic units to an account.

### Signature

```
fundFromFaucet(account, balance) => Promise<void>
account is a Reach account object.
balance is a number of tokens in atomic units with BigNumber applied.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balance = stdlib.parseCurrency(10);
  const account = await stdlib.newTestAccount(balance);
  await stdlib.fundFromFaucet(account, 10);
  console.log(`Account balance: ${await stdlib.balanceOf(account)} ${stdlib.atomicUnit}.`);
})();
```

### Output

```
Account balance: 10000010 μALGO.            # REACH_CONNECTOR_MODE=ALGO
Account balance: 10000000000000000010 WEI.  # REACH_CONNECTOR_MODE=ETH
```

## ge

This method determines if one integer is greater than or equal to another integer.

### Signature

```
  ge(x, y) => tf
  x is an integer.
  y is an integer.
  tf is a boolean.
  ```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  let x = 4;
  [-4, 4, 8].forEach((y) => {
    console.log(`${x} >= ${y} is ${stdlib.ge(x, y)} (${x >= y})`)
  });
})();
```

### Output

```
4 >= -4 is true (true)
4 >= 4 is true (true)
4 >= 8 is false (false)
```

## getDEBUG

This method returns true/false if the REACH_DEBUG environment variable is set to true/false.

### Signature

```
getDEBUG() => tf
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(stdlib.getDEBUG());
})();
Run this example with one of the following commands:

$ REACH_CONNECTOR_MODE=ALGO REACH_DEBUG=false reach run
$ REACH_CONNECTOR_MODE=ALGO REACH_DEBUG=true reach run
$ REACH_CONNECTOR_MODE=ETH REACH_DEBUG=false reach run
$ REACH_CONNECTOR_MODE=ETH REACH_DEBUG=true reach run
```

### Output

```
false or true
```

## getDefaultAccount

This method returns the Reach account object associated with the default consensus NetworkAccount. If your program is running in a browser, then the Reach account object is associated with a wallet (e.g. MetaMask or AlgoSignor). If running in a Node.js environment while connected to a Reach devnet, then the Reach account object is the faucet for the devnet. The method throws an exception if there is no default NetworkAccount defined.

### Signature

```
getDefaultAccount() => Promise<account>
account is a Reach account object.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const account = await stdlib.getDefaultAccount();
  const faucet = await stdlib.getFaucet();
  console.log(`Balance: ${await stdlib.balanceOf(account)} ${stdlib.atomicUnit}.`);
  console.log(`Balance:  ${await stdlib.balanceOf(faucet)} ${stdlib.atomicUnit}.`);
})();
```

### Output

```
# REACH_CONNECTOR_MODE=ETH
Balance: 115792089237316195423570985008687907853269984665640564038997584007913129821763 WEI.
Balance: 115792089237316195423570985008687907853269984665640564038997584007913129821763 WEI.
```

## getFaucet

This method returns the faucet for the devnet.

### Signature

```
getFaucet() => Promise<faucet>
faucet is a Reach account object.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const account = await stdlib.getDefaultAccount();
  const faucet = await stdlib.getFaucet();
  console.log(`Account balance: ${await stdlib.balanceOf(account)} ${stdlib.atomicUnit}.`);
  console.log(`Faucet balance:  ${await stdlib.balanceOf(faucet)} ${stdlib.atomicUnit}.`);
})();
```

### Output

```
# REACH_CONNECTOR_MODE=ETH
Account balance: 115792089237316195423570985008687907853269984665640564038997584007913129821763 WEI.
Faucet balance:  115792089237316195423570985008687907853269984665640564038997584007913129821763 WEI.
```

## getNetworkTime

This method returns (for ALGO and ETH) the current block number.

### Signature

```
getNetworkTime() => Promise<time>
time is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(`Network time is ${await stdlib.getNetworkTime()}.`);
})();
```

### Output

```
# ALGO
Network time is 127573.

# ETH
Network time is 236.
```

## getProvider

This method returns the current Reach provider object.

### Signature

```
getProvider() => Promise<provider>
provider is a network-specific Reach provider object.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(await stdlib.getProvider());
})();
```

## gt

This method determines if one integer is greater than another integer.

### Signature

```
gt(x, y) => tf
x is an integer.
y is an integer.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  let x = 4;
  [-4, 4, 8].forEach((y) => {
    console.log(`${x} > ${y} is ${stdlib.gt(x, y)} (${x > y})`)
  });
})();
```

### Output

```
4 > -4 is true (true)
4 > 4 is false (false)
4 > 8 is false (false)
```

## hexToBigNumber

This method converts hexadecimal string into a a BigNumber.

### Signature

```
hexToBigNumber(s) => bn
s is a string representing a hexadecimal number. 0x is optional.
bn is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const s1 = '123abc';
  const bn1 = stdlib.hexToBigNumber(s1);
  console.log(`${typeof s1}   ${s1} to ${bn1.constructor.name} ${bn1}`);
  const s2 = '0x123abc';
  const bn2 = stdlib.hexToBigNumber(s2);
  console.log(`${typeof s2} ${s2} to ${bn2.constructor.name} ${bn2}`);
})();
```

### Output

```
string   123abc to BigNumber 1194684
string 0x123abc to BigNumber 1194684
```

## hexToString

This method converts an array of hexadecimal ascii values to a string.

### Signature

```
hexToString(h) => s
h is a string representing s as an array of hex ascii values (e.g. "0x7265616368").
s is a string (e.g. "reach").
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const h = '0x7265616368';
  const s = stdlib.hexToString(h);
  console.log(`${typeof h} "${h}" to ${typeof s} "${s}".`);
})();
```

### Output

```
string "0x7265616368" to string "reach".
```

## isBigNumber

This method determines whether a value is of type BigNumber.

### Signature

```
isBigNumber(v) => tf
v is a value of some type.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  [2, stdlib.bigNumberify(2), '2', 'two'].forEach(v => {
    console.log(`${v} is ${stdlib.isBigNumber(v) ? '' : 'NOT '}a BigNumber.`);
  });
})();
```

### Output

```
2 is NOT a BigNumber.
2 is a BigNumber.
2 is NOT a BigNumber.
two is NOT a BigNumber.
```

## isHex

This method determines whether a string is a hexadecimal ascii array (e.g. "0x7265616368").

### Signature

```
isHex(s) => tf
s is a string.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  ['0x7265616368', '7265616368'].forEach(v => {
    console.log(`"${v}" is ${stdlib.isHex(v) ? '' : 'NOT '}a hex ascii array.`);
  });
})();
```

### Output

```
"0x7265616368" is a hex ascii array.
"7265616368" is NOT a hex ascii array.
```

## le

This method determines if one integer is less than or equal to another integer.

### Signature

```
le(x, y) => tf
x is an integer.
y is an integer.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  let x = 4;
  [-4, 4, 8].forEach((y) => {
    console.log(`${x} <= ${y} is ${stdlib.le(x, y)} (${x <= y})`)
  });
})();
```

### Output

```
4 <= -4 is false (false)
4 <= 4 is true (true)
4 <= 8 is true (true)
```

## lt

This method determines if one integer is less than another integer.

### Signature

```
lt(x, y) => tf
x is an integer.
y is an integer.
tf is a boolean.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  let x = 4;
  [-4, 4, 8].forEach((y) => {
    console.log(`${x} < ${y} is ${stdlib.lt(x, y)} (${x < y})`)
  });
})();
```

### Output

```
4 < -4 is false (false)
4 < 4 is false (false)
4 < 8 is true (true)
```

## makeDigest

## makeRandom

## mapRef

## mkAddressEq

## mod

This method performs integer modulus, returning the remainder of integer division.

### Signature

```
mod(x, y) => mod
x must be an integer.
y must be an integer.
mod is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const x = 20
  const y = 7
  const mod = stdlib.mod(x, y)
  console.log(`${x} % ${y} = ${mod}`)
})();
```

### Output

```
20 % 7 = 6
```

## mul

This method performs integer multiplication.

### Signature

```
mul(x, y) => prod
x must be an integer.
y must be an integer.
prod is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const x = 3
  const y = 7
  const prod = stdlib.mul(x, y)
  console.log(`${x} x ${y} = ${prod}`)
})();
```

### Output

```
3 x 7 = 21
```

## newAccountFromMnemonic

This method, given a string representing a network-specific mnemonic phrase, creates and returns a Reach account object with a balance of zero network tokens.

### Signature

```
newAccountFromMnemonic(phrase) => Promise<account>
phrase is a string for a network-specific mnemonic phrase.
account is a Reach account object.
```

Here is an example Ethereum Mnemonic object, including the phrase property:

```
{
  phrase: 'museum pass catch idea van nice letter glove jaguar coast arch pipe',
  path: "m/44'/60'/0'/0/0",
  locale: 'en'
}
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balance = stdlib.parseCurrency(10);
  const account = await stdlib.newTestAccount(balance);
  const mnemonic = account.networkAccount._mnemonic();
  console.log(mnemonic);
  const account2 = await stdlib.newAccountFromMnemonic(mnemonic.phrase);
  console.log(account2);
})();
```

## newAccountFromSecret

## newTestAccount

This method creates and returns a Reach account object with the given balance in atomic units.

### Signature

```
newTestAccount(balance) => Promise<account>
balance is a BigNumber of tokens in atomic units.
account is a Reach account object.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balance = stdlib.parseCurrency(10);
  const account = await stdlib.newTestAccount(balance);
  console.log(account);
})();
```

## objectMap

## parseCurrency

This method converts a network token amount from standard units to atomic units.

### Signature

```
parseCurrency(suAmount) => auAmount
suAmount is a number of tokens in standard units
auAmount is BigNumber of tokens in atomic units
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const suAmount = 100.5;
  const auAmount = stdlib.parseCurrency(suAmount);
  console.log(`Standard Unit Amt = ${suAmount} ${stdlib.standardUnit}`)
  console.log(`Atomic Unit Amt   = ${auAmount} ${stdlib.atomicUnit}`)
})();
```

### Output

```
# REACH_CONNECTOR_MODE=ALGO
Standard Unit Amt = 100.5 ALGO
Atomic Unit Amt   = 100500000 μALGO

# REACH_CONNECTOR_MODE=ETH
Standard Unit Amt = 100.5 ETH
Atomic Unit Amt   = 100500000000000000000 WEI
```

## parseFixedPoint

## parseInt

## protect

## providerEnvByName

## randomUInt

This method generates a random BigNumber.

### Signature

```
randomUInt() => r
r is a BigNumber representing a random number.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  for (let i = 0; i < 5; i++) {
    console.log(`Random #${i+1}: ${stdlib.randomUInt()}`);
  }
})();
```

### Output

```
# ALGO
Random #1: 17830142387684920417
Random #2: 17746974019939158498
Random #3: 9160898625104005994
Random #4: 3613411261652842147
Random #5: 983361309112715662

# ETH
Random #1: 27607848804729973581014410502293711971761448598557376981283183717322704017433
Random #2: 60037153247786503166971761128506669526562161409390884377433244834184239865070
Random #3: 58769759163361652927045305807023328094637415231145708005951464780084755561144
Random #4: 35785320426544831645097143845375516087094351111486972004275998413013072818256
Random #5: 38153054402165990843880482716988449931280675794450268073745162749585985896245
```

## setDEBUG

## setFaucet

## setProvider

## setProviderByEnv

## setProviderByName

## stringToHex

This method converts a string to an array of hexadecimal ascii values.

### Signature

```
stringToHex(s) => h
s is a string (e.g. "reach").
h is a string representing s as an array of hex ascii values (e.g. "0x7265616368").
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const s = 'reach';
  const h = stdlib.stringToHex(s);
  console.log(`${typeof s} "${s}" to ${typeof h} "${h}".`);
})();
```

### Output

```
string "reach" to string "0x7265616368".
```

## sub

This method performs integer subtraction.

### Signature

```
sub(x, y) => diff
x must be an integer.
y must be an integer.
diff is a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const x = 3
  const y = 7
  const diff = stdlib.sub(x, y)
  console.log(`${x} - ${y} = ${diff}`)
})();
```

### Output

```
3 - 7 = -4
```

## transfer

This method transfers network tokens in atomic units from one account to another. The source account pays any consensus network fees. If you do not provide a token parameter, then the transfer unit is network tokens. Otherwise, the transfer unit is the designated token type.

### Signature

```
transfer(srcAccount, dstAccount, amount[, token]) => Promise<void>
srcAccount is a Reach account object.
dstAccount is a Reach account object.
amount is an integer of tokens in atomic units.
token is a string for type of token.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const balance = stdlib.parseCurrency(20);
  const srcAccount = await stdlib.newTestAccount(balance);
  const dstAccount = await stdlib.newTestAccount(balance);
  const iSrcBalance = await stdlib.balanceOf(srcAccount);
  const iDstBalance = await stdlib.balanceOf(dstAccount);
  const xferAmount = stdlib.parseCurrency(5);
  await stdlib.transfer(srcAccount, dstAccount, xferAmount);
  const fSrcBalance = await stdlib.balanceOf(srcAccount);
  const fDstBalance = await stdlib.balanceOf(dstAccount);
  console.log(`Src Account: ${iSrcBalance} - ${xferAmount} = ${fSrcBalance} ${stdlib.atomicUnit}.`);
  console.log(`Dst Account: ${iDstBalance} + ${xferAmount} = ${fDstBalance} ${stdlib.atomicUnit}.`);
})();
```

### Output

```
# ALGO
Src Account: 20000000 - 5000000 = 14999000 μALGO.
Dst Account: 20000000 + 5000000 = 25000000 μALGO.

# ETH
Src Account: 20000000000000000000 - 5000000000000000000 = 14999999999999979000 WEI.
Dst Account: 20000000000000000000 + 5000000000000000000 = 25000000000000000000 WEI.
```

## uintToBytes

This method accepts a number or BigNumber, and returns a string representing the BigNumber form of the input.

### Signature

```
uintToBytes(input) => output
input is a number or BigNumber.
output is a string representing a BigNumber.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  [5, stdlib.bigNumberify(5)].forEach(v => {
    const output = stdlib.uintToBytes(v);
    console.log(`${v.constructor.name} ${v} to ${typeof output} ${output}`);
  });
})();
```

### Output

```
Number 5 to string 0000000000000000000000000000000000000000000000000000000000000005
BigNumber 5 to string 0000000000000000000000000000000000000000000000000000000000000005
```

## verifyContract

## wait

This method returns a promise that is resolved after the specified number of consensus network time units pass. A time unit for ALGO and ETH is one block. If you provide an optional onProgress callback, then waitUntilTime invokes the callback repeatedly during the wait time, passing an object with the BigNumber keys $jsin{currentTime} and targetTime.

If you launch reach run in an isolated testing mode, this method will force network time to pass, usually by generating trivial transactions resulting in new block formation. You launch reach run in an isolated testing mode by setting REACH_ISOLATED_NETWORK=true, or by choosing a REACH_CONNECTOR_MODE that matches $NET-test-dockerized-$IMPL for all valid $NET and $IMPL. Here is an example:

```
$ REACH_CONNECTOR_MODE=ALGO REACH_ISOLATED_NETWORK=true reach run
$ REACH_CONNECTOR_MODE=ETH REACH_ISOLATED_NETWORK=true reach run
```

### Signature

```
wait(delta, onProgress) => Promise<time>
delta is a BigNumber representing time units to wait.
onProgress is an optional callback function.
time is a BigNumber representing current network time.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const startTime = await stdlib.getNetworkTime();
  console.log(`Start Time: ${startTime}`);
  const delta = 3;
  console.log(`Delta: ${delta}`);
  const actualEndTime = await stdlib.wait(delta, (status) => {
    console.log(`Current Time: ${status.currentTime}, Target Time: ${status.targetTime}`);
  });
  console.log(`Actual End Time: ${actualEndTime}`);
})();
```

### Output

```
# ALGO
Start Time: 140438
Delta: 3
Current Time: 140439, Target Time: 140441
Current Time: 140440, Target Time: 140441
Current Time: 140441, Target Time: 140441
Actual End Time: 140441

# ETH
Start Time: 245
Delta: 3
Current Time: 245, Target Time: 248
Current Time: 246, Target Time: 248
Current Time: 247, Target Time: 248
Current Time: 248, Target Time: 248
Actual End Time: 248
```

## waitUntilTime

This method returns a promise that is resolved after the consensus network reaches the specified time which, for ALGO and ETH, is the current block number. If you provide an optional onProgress callback, then waitUntilTime invokes the callback repeatedly during the wait time, passing an object with the BigNumber keys $jsin{currentTime} and targetTime.

If you launch reach run in an isolated testing mode, this method will force network time to pass, usually by generating trivial transactions resulting in new block formation. You launch reach run in an isolated testing mode by setting REACH_ISOLATED_NETWORK=true, or by choosing a REACH_CONNECTOR_MODE that matches $NET-test-dockerized-$IMPL for all valid $NET and $IMPL. Here is an example:

```
$ REACH_CONNECTOR_MODE=ALGO REACH_ISOLATED_NETWORK=true reach run
$ REACH_CONNECTOR_MODE=ETH REACH_ISOLATED_NETWORK=true reach run
```

### Signature

```
waitUntilTime(desiredEndTime, onProgress) => Promise<time>
desiredEndTime is a BigNumber.
onProgress is an optional callback function.
time is a BigNumber representing current network time.
```

### Example

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  const startTime = await stdlib.getNetworkTime();
  console.log(`Start Time: ${startTime}`);
  const delta = 3;
  console.log(`Delta: ${delta}`);
  const desiredEndTime = stdlib.add(startTime, delta);
  console.log(`Desired End Time: ${desiredEndTime}`);
  const actualEndTime = await stdlib.waitUntilTime(desiredEndTime, (status) => {
    console.log(`Current Time: ${status.currentTime}, Target Time: ${status.targetTime}`);
  });
  console.log(`Actual End Time: ${actualEndTime}`);
})();
```

### Output

```
# ALGO
Start Time: 140135
Delta: 3
Desired End Time: 140138
Current Time: 140136, Target Time: 140138
Current Time: 140137, Target Time: 140138
Current Time: 140138, Target Time: 140138
Actual End Time: 140138

# ETH
Start Time: 242
Delta: 3
Desired End Time: 245
Current Time: 242, Target Time: 245
Current Time: 243, Target Time: 245
Current Time: 244, Target Time: 245
Current Time: 245, Target Time: 245
Actual End Time: 245
```