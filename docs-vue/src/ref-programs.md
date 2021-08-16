



# {#ref-programs} Programs

This document describes the structure and content of Reach <Defn :name="programs">programs</Defn>, including
their syntactic forms,
the standard library,
and the standards of valid programs.

::: note
Get language support for Reach in your editor by visiting XXX (seclink "guide-editor-support").
:::

The rest of this section is structured according to the contexts of the different parts of a Reach program, as follows:

+ XXX (Secref "ref-programs-valid") describes what is meant by the term valid in Reach.
+ XXX (Secref "ref-programs-module") describes the top-level structure of Reach module.
+ XXX (Secref "ref-programs-appinit") describes the structure of Reach application initialization.
+ XXX (Secref "ref-programs-step") describes the structure of Reach steps.
+ XXX (Secref "ref-programs-local") describes the structure of Reach local steps.
+ XXX (Secref "ref-programs-consensus") describes the structure of Reach consensus steps.
+ XXX (Secref "ref-programs-compute") describes the common structure of Reach computations shared by all contexts.


XXX (Figure-ref "fig:app-steps") shows the relationship between the modes of a Reach application.

XXX (figure
 "fig:app-steps"
 (elem "The modes of a Reach application")
 (image "images/reference/StepDiagram.png" #:style "fig"))

## {#ref-programs-valid} Validity and other concepts

Reach imposes further restrictions on syntactically well-formed programs.
These restrictions are described throughout this manual using the term <Defn :name="valid">valid</Defn> to refer to constructions that obey the restrictions,
and the term <Defn :name="invalid">invalid</Defn> to refer to constructions that do not obey them.

It is always invalid to use a value with an operation for which it is undefined.
For example, `1 + true` is invalid.
In other words, Reach enforces a static type discipline.

### Security levels and scope

The text of Reach program is public knowledge to all participants.
However, any value that comes from an interaction expression is a <Defn :name="secret">secret</Defn> which only that participant knows.
Furthermore, any values derived from secret values are also secret.
A value, X, is considered derived from another, Y, if the value of Y is provided to a primitive operation to arrive at X, or if Y is used as part of a conditional that influences the definition of X.
Secrets can only be made public by using the declassify primitive.

When secret values are bound to an identifier
within a local step,
the identifier name MUST be prefixed by an underscore (`_`).

When public values are bound to an identifier,
regardless of context,
the identifier name MUST NOT be prefixed by an underscore (`_`).

Consequently, identifiers which appear inside of a
function definition or arrow expression
MAY be prefixed by an underscore.
This will cause a compiler error if any value bound to that
identifier is public.

### Domination

A term Y is said to be "<Defn :name="dominated">dominated</Defn>" by a term X if all paths in the control-flow graph of the application from the root to Y pass through X.
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


`f` dominates `p`, `g`, `h`, and `m`.
But no other term dominates any other term.
In particular, `g` does not dominate `m` because it is possible to reach `m` without going through `g`, such as when `p()` is false.







