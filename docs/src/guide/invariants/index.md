# {#guide-invariants} Writing Invariants

Invariants are an essential feature when writing complex Reach DApps. 
Our guide @{seclink("guide-loop-invs")} provides a deep theoretical understanding of loop invariants, and the @{seclink("tut")} and @{seclink("tut-rsvp")} tutorials both feature invariants. If you desire to create a complex Reach DApp, then you'll likely need to write your own invariants, as well.

To help prepare you to write your own invariants, we'll review a few examples and break them to help understand the error messages. 
At the end of the guide you'll have a respect for how important invariants are in Reach Applications and how important it is to have a complete understanding of your application before writing the codebase. 

First, a few basics about invariants. 

Invariant refers to something that is constant without variation. 

Loop invariants are required when writing Reach `{!rsh} while` loops.
They are established immediately before the loop begins. 
You are allowed to write as many `{!rsh} invariant` statements as needed. 
Inside a `{!rsh} parallelReduce`, use dot notation to place each `{!rsh} invariant` on its own line to easily distinguish each `{!rsh} invariant` from the other.

The `{!rsh} while` loop condition and the body of the loop must uphold the `{!rsh} invariant`. 
To expand, there are moments within the loop that the invariant may be invalidated, but by the end of the loop, the invariant must be true. 

The invariant establishes properties for the rest of the program after the loop ends.
So the invariants establish truth just before the loop, uphold that truth inside the loop, and remains true after the loop ends.

Let's look at some examples of invariants and break them to understand their importance. 

``` rsh
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 59-61
```

This is from the @{seclink("tut")} tutorial. 
The while loop features two invariants. 
The first controls the balance in relation to the wager amount and the second ensures that the outcome is one of the enumerated outcomes from the top of the program.

In this example, if the balance ever becomes something other than double the amount of the wager then the verification engine will throw an error. 
Alternatively, if the outcome of the game is anything other than `A_WINS`, `B_WINS`, or a `DRAW`, then a different error will be thrown. 

The `{!rsh} while` loop's condition is dependent on the `outcome` being equal to `DRAW`. 

The invariant indicates the importance of tracking the balance and the condition(s) of the loop. 
In almost all cases, you'll want to track these items inside a `{!rsh} while` loop. 

Let's remove the balance from the invariant and observe the output when we execute a `{!cmd} $ reach compile`.

Our snippet now looks like: 

``` rsh
  var outcome = DRAW;
  invariant( isOutcome(outcome) );
  while ( outcome == DRAW ) {
```

Without tracking the balance, instead of compiling successfully, the verification engine now outputs:

```
Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
Verification failed:
  when ALL participants are honest
  of theorem: assert
  msg: "balance sufficient for transfer"
  at ./index.rsh:94:31:application

  // Violation Witness

  const UInt.max = 4;

  const wager/297 = "Alice".interact.wager;
  //    ^ could = 1
  //      from: ./index.rsh:30:14:property binding
  const netBalance/330 = <loop variable>;
  //    ^ could = 0
  //      from: ./index.rsh:61:5:while

  // Theorem Formalization

  const v452 = (2 * wager/297) <= netBalance/330;
  //    ^ would be false
  assert(v452);
  
  Verification failed:
  when ALL participants are honest
  of theorem: assert
  msg: "balance zero at application exit"
  at ./index.rsh:27:30:compileDApp

  // Violation Witness

  const UInt.max = 6;

  const wager/297 = "Alice".interact.wager;
  //    ^ could = 0
  //      from: ./index.rsh:30:14:property binding
  const netBalance/330 = <loop variable>;
  //    ^ could = 1
  //      from: ./index.rsh:61:5:while

  // Theorem Formalization

  const v456 = 0 == (netBalance/330 - (2 * wager/297));
  //    ^ would be false
  assert(v456);

  Verifying when NO participants are honest
Checked 77 theorems; 4 failures (and 2 omitted repeats) :'(

For a guide to understanding verification failures, see: https://docs.reach.sh/rsh/errors/#how-to-read-verification-failures
```

The error message indicates that the balance is not sufficient for a transfer on line 94. 
This line is far from our `{!rsh} while` loop and invariant. 

Line 94 is where we transfer the winnings to Alice or Bob based on who won the round.

``` rsh
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 93-95
```

If you don't understand the flow of the balance within your application then this error may be confusing. 
Even though the trouble begins at the loop invariant in line 60, the error is triggered at line 94 because this is the first time we try to move the balance out of the contract. 

Reading the Violation Witness may be more helpful in this situation. 
(If you'd like to gain a better understanding of Violation Witnesses and Theorem Formalizations then read our guide, @{seclink("how-to-read-verification-failures")}).
The verification engine is showing two possible places where the insufficient balance could have first occurred. 

The first place could be from the `wager` property binding in the Participant Interact Interface. 
The second indicates that the insufficient balance could be originating in the while loop in line 61.
The next thing we see is the Theorem Formalization where the verification engine is showing the math of how the program fails.

If you didn't already know that the balance was failing in the while loop then you'd want to investigate the lines that the Violation Witness points to. 

We see a second verification failure that indicates that the balance is not zero when the application exits. 
This verification fails at line 27, which is where the Participant Interact Interface begins. 
The Violation Witness points to the same lines as the prior error. 
Again, this indicates that something may be wrong in our `wager` property or in the `{!rsh} while` loop.

If you can rule out an error in the Participant Interact Interface then consider what invariants may still need to be tracked.
If you haven't created an invariant for the balance and the condition(s) of the loop then it is possible that your current invariants may be insufficient for the verification engine to formally verify your application.

Let's replace the balance invariant and remove the condition's invariant. 

Our code now looks like: 

``` rsh
  var outcome = DRAW;
  invariant( balance() == 2 * wager );
  while ( outcome == DRAW ) {
```

The output now reads:

```
Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
Verification failed:
  when ALL participants are honest
  of theorem: assert
  at ./index.rsh:93:15:application

  // Violation Witness

  const UInt.max = 6;

  const outcome/321 = <loop variable>;
  //    ^ could = 3
  //      from: ./index.rsh:61:5:while

  // Theorem Formalization

  const v446 = ((outcome/321 == 2) ? true : (outcome/321 == 0));
  //    ^ would be false
  assert(v446);

  Verifying when NO participants are honest
Checked 77 theorems; 2 failures (and 1 omitted repeats) :'(

For a guide to understanding verification failures, see: https://docs.reach.sh/rsh/errors/#how-to-read-verification-failures
```

In this instance, the theorem fails on an `{!rsh} assert` on line 93. 
We also read that there is one Violation Witness that points to the `{!rsh} while` loop. 
The Theorem Formalization shows a failing ternary involving `outcome`. 
Failing to write an invariant that tracks the loop's condition leaves the program open to violating static `{!rsh} assert`ions. 

Recall that the failure points to line 93 where we `{!rsh} assert` that `outcome` must be `A_WINS` or `B_WINS`. 

``` rsh
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 93-93
```

The verification engine is only able to formally verify the things inside a while loop through invariants. 
By establishing invariants for the condition and the balance the verification engine is able to formally verify the entire application. 
Without proper invariants the application cannot be formally verified and will fail to properly compile. 

Let's look at the RSVP application. 
This DApp is more complicated than the @{seclink("tut")} tutorial, but the same principles apply. 
We'll write invariants for the conditions of the `{!rsh} while` loop and the balance. 
The one difference in this application is that one of the conditions, `done`, will be verified with a `check`, which is a dynamic assertion, rather than an invariant. 

``` rsh
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 44-54
```

In RSVP, the `{!rsh} parallelReduce` tracks two constants, `done` and `howMany`.
The first invariant ensures that the size of the `{!rsh} Map`, `Guests`, is equal to `howMany`. 
If we remove this invariant, the compiler will not be able to verify that the size of the Guestbook is the same size as how many guests have reserved or checked in. 

Let's view the compiled output when this first invariant is removed:

```
Verification failed:
  when ALL participants are honest
  of theorem: assert
  msg: "balance sufficient for transfer"
  at ./index.rsh:69:33:application
  at /app/index.rsh:66:25:application call to [unknown function] (defined at: /app/index.rsh:66:25:function exp)

  // Violation Witness

  const UInt.max = 1;

  const details/713 = "Admin".interact.details;
  //    ^ could = {deadline: 1, host: <abstract address 1>, name: "Bytes!val!1", reservation: 1 }
  //      from: ./index.rsh:13:12:property binding
  const netBalance/744 = <loop variable>;
  //    ^ could = 0
  //      from: ./index.rsh:45:19:while

  // Theorem Formalization

  const v928 = details/713.reservation <= netBalance/744;
  //    ^ would be false
  assert(v928);

  Verifying when NO participants are honest
Checked 38 theorems; 3 failures (and 2 omitted repeats) :'(

For a guide to understanding verification failures, see: https://docs.reach.sh/rsh/errors/#how-to-read-verification-failures
```

The verification engine fails because the balance may be insufficient for a `{!rsh} transfer`.
Similar to before, the theorem failure points further in the program when a `{!rsh} transfer` is attempted. 
We also see that the Violation Witness points to the Admin's details property in the Participant Interact Interface and the start of the `{!rsh} while` loop.

The Theorem Formalization shows a false relation to the reservation details and the balance. 
If we didn't know the origination of this error then we would need to consider any static assertions that we may have overlooked. 
Writing your assumptions about the application before crafting the code makes it easier to complete a `{!rsh} parallelReduce`

Removing the second invariant outputs a similar balance transfer error, and an additional "balance zero at application exit" error. 

```
Verification failed:
  when ALL participants are honest
  of theorem: assert
  msg: "balance sufficient for transfer"
  at ./index.rsh:69:33:application
  at /app/index.rsh:66:25:application call to [unknown function] (defined at: /app/index.rsh:66:25:function exp)

  // Violation Witness

  const UInt.max = 2;

  const details/728 = "Admin".interact.details;
  //    ^ could = {deadline: 1, host: <abstract address 1>, name: "Bytes!val!1", reservation: 2 }
  //      from: ./index.rsh:13:12:property binding
  const netBalance/759 = <loop variable>;
  //    ^ could = 1
  //      from: ./index.rsh:45:19:while

  // Theorem Formalization

  const v948 = details/728.reservation <= netBalance/759;
  //    ^ would be false
  assert(v948);

Verification failed:
  when ALL participants are honest
  of theorem: assert
  msg: "balance zero at application exit"
  at ./index.rsh:77:7:application

  // Violation Witness

  const UInt.max = 1;

  const netBalance/759 = <loop variable>;
  //    ^ could = 1
  //      from: ./index.rsh:45:19:while

  // Theorem Formalization

  const v962 = 0 == netBalance/759;
  //    ^ would be false
  assert(v962);

  Verifying when NO participants are honest
Checked 38 theorems; 5 failures (and 3 omitted repeats) :'(

For a guide to understanding verification failures, see: https://docs.reach.sh/rsh/errors/#how-to-read-verification-failures
```

We saw the "balance zero at application exit" verification failure in our Rock, Paper, Scissors example when we removed the balance logic from the invariant. 

Again, we can see that the Violation Witnesses point to the Participant Interact Interface and the `{!rsh} while` loop. 
The Theorem Formalizations show errors in calculating the balance. 
If we saw these errors in our application then we might reconsider how we expect the balance to be calculated and identify missing assumptions based on our findings. 
In this case, the balance is equal to the product of `howMany` guests completed a reservation and the reservation fee. 

At this point, we've examined four invariants and how their absence results in Violation Witness errors. 
This demonstrates how important it is to fully understand your program before it is time to write `{!rsh} invariant`s.
If you'd like more practice before writing, continue this exercise of finding examples, removing invariants, and studying the resulting compile errors. 

If you'd like to experiment with invariants in additional examples then I recommend starting with the [NFT-Auction-API](https://github.com/reach-sh/reach-lang/blob/master/examples/nft-auction-api/index.rsh) and the [Chicken-Parallel](https://github.com/reach-sh/reach-lang/blob/master/examples/chicken-parallel/index.rsh) examples. 

This can be a good way of developing a sense of when and how to use invariants. 
Common error messages you may see are "balance zero at application exit" and "balance sufficient for transfer". 
If you see these errors in your applications, take time to consider the balance throughout the application. 
Seek to identify a mathematical relationship between loop conditions and the balance.

You should now have a better understanding of how to write invariants and their importance in developing secure Reach DApps.
If you need additional advice for your DApp, reach out in our [Discord](@{DISCORD}).