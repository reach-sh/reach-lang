# {#guide-api-syntax} API Syntax

An API in a Reach back-end is a set of functions that are not bound to any participant.
It means that unlike participant interfaces, where only the user with the right role can respond to the messages for a specific participant, API functions can be called by anyone.

To declare an API role, use `API(name, interface) => API_Participant` function

e.g. `const Spectator = API('Spectator', { rate: Fun([UInt], Bool) })`

Each function in the API interface is declared so that domain is provided by API and return value is the response of the contract. 
In this case by calling `Spectator.rate`, API will send an unsigned integer to the contract and receive a boolean.

There are two ways to call an API function:
1. `call()` function
2. `.api()` function in a fork statement

## `call()`

`call( apiFn )` is a function to let Reach know that we're waiting for an API function call.
Inside the function we provide the handle of function.

e.g. `const [[inputFromAPI], setReturnValue] = call(Spectator.rate)`

Return value from a `call` is a tuple where
- The first element is the domain of the function, i.e. the arguments function is called with. 
- The second element is a setter function to respond to the API

In this case if `Spectator.rate()` is called with the value `5`, result tuple would be `[[5], setterFunction]`. 

And `setterFunction(true)` would return `true` to the API.

By means of program steps, a call counts as a publishment and can be chained with these functions:
- `pay((<domain>) => <payment_amount>)` makes the API pay the `<payment_amount>`

```reach
const [[rating], setResponse] = call(Spectator.rate)
  .pay((rating) => rating)
```

- `assume((<domain>) => <condition>)` makes sure honest participants have the `<condition>` true.

```reach
const [[rating], setResponse] = call(Spectator.rate)
  .assume((rating) => rating < 10)
```

- `throwTimeout(<time>, <throw_expression>)` throws an error with arguments of `<throw_expression>` if function in not called for `<time>`

```reach
try {
  const [[rating], setResponse] = call(Spectator.rate)
    .throwTimeout(1024, 0)
}
catch (e) { ... }
```

An example implementation of `call` would be:
```reach
// Alice is implemented before
Alice.only(() => { doFlip() });
Alice.publish();
commit();

const [[rating], setResponse] = call(Spectator.rate)
  .assume((rating) => rating < 10);

Alice.interact.seeRating(rating);
setResponse(true);
commit();
```


 
## `.api()`

`.api()` can be appended to any fork statement to declare a case for API call.
The syntax is

```reach
fork() // or parallelReduce(INIT_EXPR)
// ...
.api(API_EXPR,
  API_ASSUME_EXPR,
  API_PAY_EXPR,
  API_CONSENSUS_EXPR)
```
where 
- `API_EXPR` is the API participant function, 
- `API_ASSUME_EXPR` is a function that takes *the call domain* and returns a condition boolean, which honest participants must comply. **IS OPTIONAL** 
- `API_PAY_EXPR` is a function that takes *the call domain* and returns a payment amount for caller to pay. **IS OPTIONAL**
- `API_CONSENSUS_EXPR` is a function that takes *the call domain and setter function*, which then makes consensus transfers.

An example implementation would be:
```reach
// PRICE_PER_RATING declared somewhere before

const totalScore = parallelReduce( 0 ) 
// ...
api(
  Spectator.rate,
  ((rating) => { assume(rating < 10) }),
  ((rating) => rating * PRICE_PER_RATING)
  ((rating, setResponse) => {
     setResponse(true);
     return totalScore + rating;
  })
); 
```

## Front-end Interaction

API functions are stored inside `contract.apis` or `contracts.a` where each API participant are stored as a field.

`contracts.apis.Spectator` would return `{ rate: <async_function> }`

```js
// ...
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [deployer] = await stdlib.newTestAccount(startingBalance);

const contract = deployer.contract(backend);

try { 
 const res = await contract.apis.Spectator.rate(5);
} 
catch (e) {
 console.error("Error while calling Spectator.rate");
}
// ...
```
