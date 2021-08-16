



# {#ref-programs-local} Local Steps

A Reach local step occurs in the body of `only` or `each` statements.
It represents the actions taken by a single participant in an application.

## {#ref-programs-local-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a local step.

## {#ref-programs-local-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-compute-exprs) are valid for a local step.
However, some additional expressions are allowed.

### {#ref-programs-local-this} `this`

Inside of a local step, `this` refers to the participant performing the step.
This is useful when the local step was initiated by an `each` expression.

### `interact`

<Ref :name="(quote rsh):interact" />
```reach
interact.amount
interact.notify(handA, handB)
interact.chooseAmount(heap1, heap2) 
```


An <Defn :name="interaction expression">interaction expression</Defn>, written `interact.METHOD(EXPR_0, ..., EXPR_n)`, where `METHOD` is an identifier bound in the participant interact interface to a function type, and `EXPR_0` through `EXPR_n` are expressions that evaluates to the result of an interaction with a frontend that receives the evaluation of the `n` expressions and sends a value.

An interaction expression may also be written `interact.KEY`, where `KEY` is bound in the participant interact interface to a non-function type.

An interaction expression may only occur in a local step.

### `assume`

<Ref :name="(quote rsh):assume" />
```reach
assume( claim, [msg] ) 
```


 An assumption where `claim` evaluates to `true` with honest frontends.
This may only appear in a local step.
It accepts an optional bytes argument, which is included in any reported violation.

### `fail`

<Ref :name="(quote rsh):fail" />
```reach
fail() 
```


 is a convenience method equivalent to `assume(false)`. This may only appear in a local step.

### `declassify`

<Ref :name="(quote rsh):declassify" />
```reach
declassify( arg ) 
```


The <Defn :name="declassify">declassify</Defn> primitive performs a declassification of the given argument.

### `makeCommitment`

<Ref :name="(quote rsh):makeCommitment" />
```reach
makeCommitment( interact, x ) 
```


 Returns two values, `[ commitment, salt ]`, where `salt` is the result of calling `interact.random()`, and
`commitment` is the digest of `salt` and `x`.
This is used in a local step before `checkCommitment` is used in a consensus step.


