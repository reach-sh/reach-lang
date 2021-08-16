



# {#ref-frontends-rpc-py} Python (RPC)

::: note
This frontend library relies on the [Reach RPC Server](##ref-backends-rpc).
:::

A [Python](https://www.python.org) client library for the
[Reach RPC protocol](##ref-backends-rpc) may be installed by running:
```
$ pip install --upgrade reach-rpc-client
```


Once installed, add the following import line to your Python file which will
connect to the XXX (seclink "ref-backends-rpc"):
```py
from reach_rpc import mk_rpc
```


The library provides the following bindings:

---
<Ref :name="(quote py):mk_rpc" />
```py
rpc, rpc_callbacks = mk_rpc(opts)
```


`mk_rpc` accepts the XXX (secref "ref-backends-rpc-opts") as a dictionary and returns two functions, traditionally called `rpc` and `rpc_callbacks`.

<Ref :name="(quote py):rpc" />
`rpc` is a function that invokes a synchronous value RPC method.
It takes a string, naming the RPC method, and some JSON values to provide as arguments.
It returns a single JSON value as the result.

For example,

```py
rpc('/stdlib/formatCurrency', i, 4)
```


calls `formatCurrency` with some value `i` and `4`.

<Ref :name="(quote py):rpc_callbacks" />
`rpc_callbacks` is a function that invokes an interactive RPC method, such as for a backend.
It takes a string, naming the RPC method, a JSON value as an argument,
and dictionary from strings to JSON values or functions.
The functions will be provided as interactive RPC callbacks to the
RPC method and should expect JSON values as arguments and return a JSON
value as a result.
It does not return a value.

For example,

```py
def showX(x):
    print('Alice saw that X is %s'
          % rpc('/stdlib/bigNumberToNumber', x))

ms = { 'price': 10,
       'showX': showX,
     }
rpc_callbacks("/backend/Alice", ctc, ms)
```


calls a backend named `Alice` with the contract `ctc` and a value named `price` and a method named `showX` that prints out a result from the Reach backend.

