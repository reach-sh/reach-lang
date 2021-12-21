


# {#guide-assert} How and what to verify

Reach's verification engine ensures that invariants about the state of a program assumed by programmers are held by all possible executions of the program.

At a high-level, the goal of a programmer getting started with verification is to write down _every single assumption they have_ into the program in the form of `{!rsh} assert` statements.
For example, if a value, `{!rsh} x` is assumed to be smaller than 20, then the programmer should always include `{!rsh} assert(x < 20);` in the program.
This is not to help the verification engine prove later properties, but is to give the verification engine assumptions that it can attempt to falsify so the programmer can learn if their assumptions are correct.

At a low-level, the programmer should see the verification engine as a tool to prevent test regressions by encoding tests directly into the program in the form of assertions.
For example, suppose that during development and testing, a programmer observes an erroneous state where the variable `{!rsh} y` is assigned to the value `{!rsh} 41`, then the programmer should insert `{!rsh} assert(y != 41);` into the program.
The programmer should insert this check _before_ they fix the problem in the code.
This will ensure that all future versions of the program will also be protected from these problems.

These high- and low-level perspectives on assertions apply to individual code fragments, like the body of an `{!rsh} only` statement, as well as entire functions.
For example, if a programmer expects a unary function over integers, `{!rsh} f`, to always return a number between `{!rsh} 0` and `{!rsh} 50`, then they should write `{!rsh} assert(f(forall(UInt)) <= 50);` in their program.
Similarly, the unit tests for a function that a developer would normally write in a test suite, should instead be written as a series of assertions in the module that defines a function.

If you'd like to continue learning about verification, we recommend reading about ["property-based testing"](https://duckduckgo.com/?q=property-based+testing).
Although most resources on the topic will refer to dynamic, random tools, like [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck), the strategies used transfer automatically to a formally verified context, like Reach.

If you'd like to continue reading about verification in Reach specifically, read [the guide section on loop invariants](##guide-loop-invs).
