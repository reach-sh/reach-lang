# {#guide-invariants} Writing Invariants

Invariants are an essential feature when writing complex Reach DApps. 
If you're reading this guide then you've probably encountered `{!rsh} invariant`s in other parts of the documentation. 
Our guide @{seclink("guide-loop-invs")} provides a deep theoretical understanding of loop invariants, and the @{seclink("tut")} and @{seclink("tut-rsvp")} tutorials both feature invariants. 
Interesting Reach DApps require an intimate understanding of invariants.
This guide will strengthen your ability to identify what values need to be restricted inside a `{!rsh} while` loop and help you write your own invariants.

To understand their importance, we'll break working invariants and review the resulting error messages. 
By the end of the guide you'll have a sense of the most common error messages that occur due to missing invariants. 
You'll also gain a respect for how important invariants are in Reach applications and how important it is to understand your application before writing the codebase. 

## Basics

Invariant refers to something that is constant without variation. 

Invariants are required in Reach `{!rsh} while` loops and in `{!rsh} parallelReduce`.
They are written immediately before the loop begins. 
You are allowed to write as many `{!rsh} invariant` statements as needed. 
Inside a `{!rsh} parallelReduce`, use dot notation to place each `{!rsh} invariant` on its own line to easily distinguish each `{!rsh} invariant` from the other.

The `{!rsh} while` loop condition and the body of the loop must uphold the `{!rsh} invariant`. 
There are moments within the loop that the invariant may be invalidated, but by the end of the loop, the invariant must be true. 

The invariant establishes properties for the rest of the program after the loop ends.
So the invariants establish truth just before the loop, uphold that truth inside the loop, and remains true after the loop ends.

## Importance of Invariants

Let's look at examples of invariants and break them to understand their importance. 

``` rsh
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 59-61
```

This is from the @{seclink("tut")} tutorial. 
The while loop features two invariants. 
The first (line 60) asserts the balance never changes and is always two times the wager. 
The second invariant, also on line 60, ensures that the outcome is one of the enumerated outcomes from the top of the program (shown below).

``` rsh
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 4-4
```

In this example, if the engineer ever modified the program in a way that the balance wasn't guaranteed to be two times the wager amount then there would be an error. 
Alternatively, if the outcome of the game is anything other than `A_WINS`, `B_WINS`, or a `DRAW`, then a different error will be thrown. 

The invariant indicates the importance of tracking the balance and the condition(s) of the loop. 
In almost all cases, you'll want to track these items inside a `{!rsh} while` loop. 

### Balance Invariant

Let's remove the balance invariant and observe the output when we execute a `{!cmd} $ reach compile`.

Our snippet now looks like: 

``` rsh
load: /examples/rps-7-loops/index-balinv.rsh
md5: 7e59eeb6efb5e26e97c4c1d4df6669c6
range: 59-61
```

The verification engine outputs:

``` txt
load: /examples/rps-7-loops/index-balinv.txt
md5: fa159888f678f8f5057e5564e8f319c1
```

The error message on line 7 indicates that the balance is not sufficient for the `{!rsh} transfer` on line 94. 
This line is far from our `{!rsh} while` loop and invariant, which originates on line 49. 

``` rsh
load: /examples/rps-7-loops/index-balinv.rsh
md5: 7e59eeb6efb5e26e97c4c1d4df6669c6
range: 93-95
```

Line 94 is where we transfer the winnings to Alice or Bob based on who won the round.

If you don't understand the flow of the balance within your application then this error may be confusing. 
Even though the trouble begins at the loop invariant in line 60, the error is triggered at line 94 because this is the first time we try to move the balance out of the contract. 

Reading the Violation Witness may be more helpful in this situation. 
(If you'd like to gain a better understanding of Violation Witnesses and Theorem Formalizations then read our guide, @{seclink("how-to-read-verification-failures")}).
The verification engine is showing two possible places where the insufficient balance could have first occurred. 

The first place could be from the `wager` property binding in the Participant Interact Interface on line 30.

``` rsh
load: /examples/rps-7-loops/index-balinv.rsh
md5: 7e59eeb6efb5e26e97c4c1d4df6669c6
range: 27-32
```

The second indicates that the insufficient balance could be originating in the while loop in line 61.

``` rsh
load: /examples/rps-7-loops/index-balinv.rsh
md5: 7e59eeb6efb5e26e97c4c1d4df6669c6
range: 60-62
```

The next thing we see is the Theorem Formalization. 
The verification engine is showing the representation of the theorem "balance sufficient for transfer" as a program.

``` txt
load: /examples/rps-7-loops/index-balinv.txt
md5: fa159888f678f8f5057e5564e8f319c1
range: 21-25
```

Essentially, this theorem representation shows that the balance would not be equal to two times the wager amount. For a deeper study on Theorem Formalization, read our guide, @{seclink("how-to-read-verification-failures")}).

If you didn't already know that the balance was failing in the while loop then you'd want to investigate the lines that the Violation Witness points to. 

``` txt
load: /examples/rps-7-loops/index-balinv.txt
md5: fa159888f678f8f5057e5564e8f319c1
range: 27-48
```

We see a second verification failure that indicates that the balance is not zero when the application exits. 
This verification fails at line 27, which is where the Participant Interact Interface begins. 

``` rsh
load: /examples/rps-7-loops/index-balinv.rsh
md5: 7e59eeb6efb5e26e97c4c1d4df6669c6
range: 27-32
```

The Violation Witness points to the same lines as the prior error. 
Again, this indicates that something may be wrong in our `wager` property on line 30 or in the `{!rsh} while` loop on line 61.

``` rsh
load: /examples/rps-7-loops/index-balinv.rsh
md5: 7e59eeb6efb5e26e97c4c1d4df6669c6
range: 59-62
```

If you can rule out an error in the Participant Interact Interface then consider what invariants may still need to be tracked.
If you haven't created an invariant for the balance and the condition(s) of the loop then it is possible that your current invariants may be insufficient for the verification engine to formally verify your application.

### Condition Invariant

Let's replace the balance invariant and remove the condition's invariant. 

Our code now looks like: 

``` rsh
load: /examples/rps-7-loops/index-condinv.rsh
md5: 607c8bd0c7fb7a49570946eb309c69df
range: 59-61
```

The output now reads:

```
load: /examples/rps-7-loops/index-condinv.txt
md5: addb49b7bdacdfad0fe4e9661c90b554
```

In this instance, the theorem fails on an `{!rsh} assert` on line 93. 

``` rsh
load: /examples/rps-7-loops/index-condinv.rsh
md5: 607c8bd0c7fb7a49570946eb309c69df
range: 93-93
```

We also read that there is one Violation Witness that points to the `{!rsh} while` loop on line 61. 
The Theorem Formalization shows a failing ternary involving `outcome`.

```
load: /examples/rps-7-loops/index-condinv.txt
md5: addb49b7bdacdfad0fe4e9661c90b554
range: 17-21
```

Failing to write an invariant that asserts the loop's condition leaves the program open to violating static `{!rsh} assert`ions, such as the one controlling the `outcome` on line 93. 

``` rsh
load: /examples/rps-7-loops/index-condinv.rsh
md5: 607c8bd0c7fb7a49570946eb309c69df
range: 93-93
```

Line 93 `{!rsh} assert`s that `outcome` must be `A_WINS` or `B_WINS`, but the compiler cannot verify this assertion without an invariant in the `{!rsh} while` loop. 

The verification engine is only able to formally verify the things inside a while loop through invariants. 
By establishing invariants for the condition and the balance the verification engine is able to formally verify the entire application. 
Without proper invariants the application cannot be formally verified and will fail to compile. 

## Invariants in paralellReduce

Let's look at the RSVP application. 
This DApp is more complicated than the @{seclink("tut")} DApp, but the same principles apply. 
We'll write invariants for the conditions of the `{!rsh} while` loop and the balance. 
The one difference in this application is that one of the conditions, `done`, will be verified with a `{!rsh} check`, which is a dynamic assertion, rather than an invariant. 

``` rsh
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 44-54
```

In RSVP, the `{!rsh} parallelReduce` tracks two constants, `done` and `howMany`.
The first invariant on line 49 asserts that the size of the `{!rsh} Map`, `Guests`, is equal to `howMany`. 
If we remove this invariant, the compiler will not be able to verify that the size of the Guestbook is the same as how many guests have reserved or checked in. 

### Map Invariant

Let's view the compiled output when this first invariant is removed:

```
load: /examples/rsvp-6-vevt/index-mapinv.txt
md5: 86c5c2187f037fb785f347ab28ccefd6
```

The verification engine fails because the balance may be insufficient for a `{!rsh} transfer` (line 7).
Similar to before, on line 8, the theorem failure points further into the program when the `{!rsh} transfer` is attempted on line 68 (shown below). 
Line 9 in the error message points to the DApp's return function that deletes `Guests` and controls the `{!rsh} transfer` logic (line 65).

``` rsh
load: /examples/rsvp-6-vevt/index-mapinv.rsh
md5: 94acafe2646ee59c45c89b6e1c7a6592
range: 65-72
```

The Violation Witness (lines 15-20) states that the Admin's details property in the Participant Interact Interface could be modified to result in an error.
It also shows that the start of the `{!rsh} while` loop could be erroneously modified by the programmer.

The Theorem Formalization shows a false relation to the reservation details and the balance. 
If we didn't know the origination of this error then we would need to consider any static assertions that we may have overlooked. 
Writing your assumptions about the application before crafting the code makes it easier to complete a `{!rsh} parallelReduce`.

### Balance Invariant

Removing the second invariant outputs a similar balance transfer error, and an additional "balance zero at application exit" error. 

``` txt
load: /examples/rsvp-6-vevt/index-balinv.txt
md5: ffa07ffcab3de8b2bb5ec92c161877e0
```

We saw the "balance zero at application exit" verification failure in our Rock, Paper, Scissors example when we removed the balance logic from the invariant. 

Again, we can see that the Violation Witnesses (above in lines 15-20) point to the Participant Interact Interface and the `{!rsh} paralellReduce`  (`index-balinv.rsh` lines 13 and 45, respectively).

```
load: /examples/rsvp-6-vevt/index-balinv.rsh
md5: 5551e68590bad318743accb1a03639a4
range: 45-50
```

The Participant Interact Interface violation reference is similar to before so we won't look at that again. 
However, it's worth pointing out that the second violation points to the beginning of the `{!rsh} parallelReduce`.
This is because a `{!rsh} parallelReduce` creates a race in a loop.
When an `{!rsh} API` is involved, as in the RSVP application, invariants must make `{!rsh} assert`ions over the conditions of the `{!rsh} parallelReduce`. 
Learn more about races and `{!rsh} parallelReduce` in our @{seclink("guide-ctransfers")} guide.

The Theorem Formalizations in lines 24-26 and 44-46 show a representation of the theorem "balance sufficient for transfer" and "balance zero at application exit" as programs, respectively.
It demonstrates how the program would error when calculating the balance. 

``` txt
load: /examples/rsvp-6-vevt/index-balinv.txt
md5: ffa07ffcab3de8b2bb5ec92c161877e0
range: 24-26
```

``` txt
load: /examples/rsvp-6-vevt/index-balinv.txt
md5: ffa07ffcab3de8b2bb5ec92c161877e0
range: 44-46
```

Balance errors indicate the need to recalculate the balance's formula or identify missing assumptions about the program. 
In this case, the balance is equal to the product of `howMany` guests completed a reservation and the reservation fee. 
However, Reach cannot guarantee the efficacy of the program because we failed to `{!rsh} assert` the balance's formula in the invariant.

## Invariably, We Have Learned

At this point, we've examined four invariants and how their absence results in Violation Witness errors. 
This demonstrates how important it is to understand your program before it is time to write `{!rsh} invariant`s.
For more practice, continue to find examples, remove the invariants, and study the resulting compile errors. 

If you'd like to experiment with invariants in additional examples then I recommend starting with the [NFT-Auction-API](https://github.com/reach-sh/reach-lang/blob/master/examples/nft-auction-api/index.rsh) and the [Chicken-Parallel](https://github.com/reach-sh/reach-lang/blob/master/examples/chicken-parallel/index.rsh) examples. 

This can be a good way of developing a sense of when and how to use invariants. 
Common error messages you may see are "balance zero at application exit" and "balance sufficient for transfer". 
If you see these errors in your applications, take time to consider the formula of the balance in your application. 
Seek to identify a mathematical relationship between loop conditions and the balance.

You should now have a better understanding of how to write invariants and their importance in developing secure Reach DApps.
If you need additional advice for your DApp, reach out in our [Discord](@{DISCORD}).