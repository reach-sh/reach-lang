# {#ref-programs} Language

This document describes the structure and content of Reach @{defn("programs")}, including
their syntactic forms,
the standard library,
and the standards of valid programs.

:::note
Get language support for Reach in your editor by visiting @{seclink("guide-editor-support")}.
:::


The rest of this section is structured according to the contexts of the different parts of a Reach program, as follows:

+ @{seclink("ref-programs-valid")} describes what is meant by the term valid in Reach.
+ @{seclink("ref-programs-module")} describes the top-level structure of a Reach module.
+ @{seclink("ref-programs-appinit")} describes the structure of Reach application initialization.
+ @{seclink("ref-programs-step")} describes the structure of Reach steps.
+ @{seclink("ref-programs-local")} describes the structure of Reach local steps.
+ @{seclink("ref-programs-consensus")} describes the structure of Reach consensus steps.
+ @{seclink("ref-programs-compute")} describes the common structure of Reach computations shared by all contexts.


The relationship between the modes of a Reach application is shown by this diagram:

![](/images/reference/StepDiagram.png)

## {#ref-programs-valid} Validity and other concepts

Reach imposes further restrictions on syntactically well-formed programs.
These restrictions are described throughout this manual using the term @{defn("valid")} to refer to constructions that obey the restrictions,
and the term @{defn("invalid")} to refer to constructions that do not obey them.

It is always invalid to use a value with an operation for which it is undefined.
For example, `{!rsh} 1 + true` is invalid.
In other words, Reach enforces a static type discipline.

### Security levels and scope

The text of a Reach program is public knowledge to all participants.
However, any value that comes from an interaction expression is a @{defn("secret")} which only that participant knows.
Furthermore, any values derived from secret values are also secret.
A value, X, is considered derived from another, Y, if the value of Y is provided to a primitive operation to arrive at X, or if Y is used as part of a conditional that influences the definition of X.
Secrets can only be made public by using the declassify primitive.

When secret values are bound to an identifier
within a local step,
the identifier name MUST be prefixed by an underscore (`{!rsh} _`).

When public values are bound to an identifier,
regardless of context,
the identifier name MUST NOT be prefixed by an underscore (`{!rsh} _`).

Consequently, identifiers which appear inside of a
function definition or arrow expression
MAY be prefixed by an underscore.
This will cause a compiler error if any value bound to that
identifier is public.

### Domination

A term Y is said to be "@{defn("dominated")}" by a term X if all paths in the control-flow graph of the application from the root to Y pass through X.
In most cases, this corresponds to "X appears above Y at the same or lower level of indentation" in the program source code.

For example, in the following program:

```reach
f();
if ( p() ) {
 g();
} else {
 h();
}
m();
```


`{!rsh} f` dominates `{!rsh} p`, `{!rsh} g`, `{!rsh} h`, and `{!rsh} m`.
But no other term dominates any other term.
In particular, `{!rsh} g` does not dominate `{!rsh} m` because it is possible to reach `{!rsh} m` without going through `{!rsh} g`, such as when `{!rsh} p()` is false.







