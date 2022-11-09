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

The invariant indicates the importance of asserting the balance and the condition(s) of the loop. 
In almost all cases, you'll want to constrain these items inside a `{!rsh} while` loop. 

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

If you can rule out an error in the Participant Interact Interface then consider what invariants may be required by the `{!rsh} while` loop.
If you haven't created an invariant for the balance and the condition(s) of the loop then it is possible that your current invariants may be insufficient for the verification engine to formally verify your application.
A pattern will emerge as we progress through this guide.
We'll see that the `{!rsh} invariant` should `{!rsh} assert` the balance, loop condition, and `{!rsh} Map` size, if applicable. 
`{!rsh} check`s within the `{!rsh} parallelReduce` relate to the invariants. 

We'll continue to explore this pattern for writing invariants. 
We've examined an `{!rsh} invariant` for asserting the `balance`. 
Now we'll review an `${!rsh} invariant` that asserts the loop's condition.

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

## Track/Distribute Supply of Non-Network Tokens

Sometimes you may want to write an `{!rsh} assert`ion regarding the supply of non-network tokens. 
In the "Ticket Sale DApp" an Administrator issues non-network tokens and Buyer `{!rsh} API`s have the ability to buy the tokens, which are referred to as 'tickets', in this DApp.

The `{!rsh} invariant` in this DApp is interesting because it makes `{!rsh} assert`s over the network token balance (line 34) and the non-network token balance (line 35).

```
load - ticket sales
md5 -
range - 33-44

  const [ticketsSold] = parallelReduce([0])
    .invariant(balance() == amount * ticketsSold)
    .invariant(balance(tok) == supply - ticketsSold)errors
    .while(ticketsSold < supply)
    .api_(B.buyTicket, () => {
      return[amount, (ret) => {
        transfer(1, tok).to(this);
        ret(true);
        return [ticketsSold + 1];
      }];
    });
  transfer(balance()).to(A);
```

The `{!rsh} parallelReduce` updates the value of `ticketsSold` and the condition is `true` as long as `ticketsSold` is less than the available `supply` of tickets. 
The `{!rsh} API` calls on the `buyTicket` function, which increments the number of `ticketsSold` and returns the new value. 

This program doesn't break after removing the first balance invariant because the network tokens aren't transferred within the `{!rsh} parallelReduce`. 
The `{!rsh} parallelReduce` is in a race to sell the non-network tokens. 
Writing an invariant about the network token balance provides stronger defenses to the program, but is not required for the compiler to complete its formal verification.
However, removing the second invariant will cause a violation witness.

The critical `{!rsh} invariant` on line 35 and `{!rsh} assert`s that the non-network token balance is equal to the difference of the supply and the number of tickets sold.

Once removed, the compiler returns

```
load - 
md5 -
range -

Verification failed:
  when ALL participants are honest
  of theorem: assert
  msg: "balance sufficient for transfer"
  at ./index.rsh:39:28:application
  at /app/index.rsh:38:28:application call to [unknown function] (defined at: /app/index.rsh:38:28:function exp)

  // Violation Witness

  const UInt.max = 1;

  const tokenInfos/169 = <loop variable>;
  //    ^ could = Array.const(Tuple(UInt, UInt, Bool), [0, 0, false ] )
  //      from: ./index.rsh:33:39:while

  // Theorem Formalization

  const v189 = 1 <= tokenInfos/169[0][0];
  //    ^ would be false
  assert(v189);
```

We see the `balance sufficient for transfer` error, once again. 
However, instead of referring to a network token transfer, the message is referring to the transfer of a non-network token. 
In this example, the message points to the `{!rsh} transfer` on line 39 and the `return` statement on line 38.

The Violation Witness on line XX shows how the `{!rsh} parallelReduce` could fail, followed by the Theorem Formalization.

The second verification failure, "balance zero at application exit" indicates that tokens could remain in the contract when it exits on line 45.
This Violation Witness also points to the `{!rsh} parallelReduce` with a similar example as before.

This example shows that it is critical to understand a non-network token's balance, as well as, the network token's balance.

## All Together Now

Let's look at one more example that asserts `{!rsh} invariant`s over network tokens, non-network tokens, and a `{!rsh} Map`.  

``` rsh
load: /examples/point-of-sale/index.rsh
md5: 102ed2e3f3a0d59a4f1a5aa5084823cf
range: 26-44
```

This is a point-of-sale Reach application that allows an `{!rsh} API` member to make a purchase or request a refund. 
An interesting feature in the point-of-sale application is that it takes in varying cost amounts and stores the inputs in a `{!rsh} Map` which is available to be returned through the refund function.

You should be able to identify that the `{!rsh} invariant`(s) assert network and, if applicable, non-network balances, the loop condition, and the `{!rsh} Map` size, if applicable. 
We also can identify a relationship between the `{!rsh} Map` `{!rsh} invariant` and the `{!rsh} check`.
`{!rsh} Map`s with transfers inside a `{!rsh} parallelReduce` require an `{!rsh} invariant` and a `{!rsh} check`. 

The other two `{!rsh} check`s are defensive. 
`check(tokensSold != supply)` on line 34 offers insurance against split-second API calls, and `check(purchasePrice > min)` on line 36 is like a `try catch` to ensure that purchase calls meet the minimum price.

### Lost Without a Map

Let's remove each of the invariants and observe the outputs.
First, we remove the `{!rsh} Map` `{!rsh} invariant`:

``` rsh
load: /examples/point-of-sale/index-mapinv.rsh
md5: 76b0af6be62a70fe1ad5cca2f5f85307
range: 27-31
```

The verification fails because the compiler cannot confirm that the balance is sufficient for a transfer. 

``` rsh
load: /examples/point-of-sale/index-mapinv.txt
md5: f28ba47027648e7e39b34e44b73cc763
range: 7-9
```

Failures point to the `{!rsh} api_` refund return object on line 46 and the `{!rsh} transfer` on line 48.

``` rsh
load: /examples/point-of-sale/index-mapinv.rsh
md5: 76b0af6be62a70fe1ad5cca2f5f85307
range: 46-48
```

Without the `{!rsh} invariant`, Reach is not able `{!rsh} assert` the size of the `{!rsh} Map`.
When an `{!rsh} API` member attempts to call a refund, the verification engine cannot guarantee that the balance will be sufficient to payout the transfer amount. 
It is critical to know the size of a `{!rsh} Map` if it will be used in a `{!rsh} transfer`. 

### Out of Balance

Removing the balance `{!rsh} invariant` has similar results:

``` rsh
load: /examples/point-of-sale/index-balinv.rsh
md5: b2c319bf236e4d7ddade29937e162e39
range: 27-31
```

As we saw before, the verification engine is not able to ensure that the balance is sufficient for a transfer in the `{!rsh} api_`'s refund functionality. 

``` rsh
load: /examples/point-of-sale/index-balinv.txt
md5: 90ef94492eaaae686c052e46a29a9373
range: 7-9
```

Removing the balance `{!rsh} invariant` causes a second failure point at line 54 when the contract transfers the `total` to the Administrator. 

``` rsh
load: /examples/point-of-sale/index-balinv.txt
md5: 90ef94492eaaae686c052e46a29a9373
range: 34-35
```

``` rsh
load: /examples/point-of-sale/index-balinv.rsh
md5: b2c319bf236e4d7ddade29937e162e39
range: 54
```

The third and final error states that the verification engine cannot confirm that the network token balance is zero when the application exits on line 56.

``` rsh
load: /examples/point-of-sale/index-balinv.txt
md5: 90ef94492eaaae686c052e46a29a9373
range: 57-58
```

``` rsh
load: /examples/point-of-sale/index-balinv.rsh
md5: b2c319bf236e4d7ddade29937e162e39
range: 56
```

`{!rsh} invariant`s are always required for balances that will be transferred inside a `{!rsh} parallelReduce`.
Failing to make an `{!rsh} assert`ion over the balance with an `{!rsh} invariant` creates a host of verification errors that could be difficult to resolve without properly understanding the application. 

### Tokens

For our final example, we remove the non-network token balance `{!rsh} invariant`.

``` rsh
load: /examples/point-of-sale/index-tokinv.rsh
md5: 2280bcf03c528e31b6c37104786dc927
range: 27-31
```

We observe that the verification failure message is the same, "balance sufficient for transfer". 
However, it fails inside the `{!rsh} api_` purchase functionality.

``` rsh
load: /examples/point-of-sale/index-tokinv.txt
md5: 557d91780cd2f51743d5b84eca1d7989
range: 7-9
```

Non-network tokens are transferred on line 39 and the non-network token balance is updated in line 36. 

``` rsh
load: /examples/point-of-sale/index-tokinv.rsh
md5: 2280bcf03c528e31b6c37104786dc927
range: 36-40
```

Similar to network tokens, transferring non-network tokens within a `{!rsh} parallelReduce` requires an `{!rsh} invariant`.

Failing to provide the non-network token `{!rsh} invariant` also creates a "balance zero at application exit" error on line 56, which is where the application `{!rsh} exit`s.

``` rsh
load: /examples/point-of-sale/index-tokinv.txt
md5: 557d91780cd2f51743d5b84eca1d7989
range: 28-29
```

All tokens must be accounted for when the application is ready to exit. 
Reach is not able to confirm balances after exiting a `{!rsh} parallelReduce` without `{!rsh} invariant`s.
If the compiler fails with "balance zero at application exit" then ensure you've created `{!rsh} invariant`s that `{!rsh} assert` the correct equations for network and non-network tokens.  

## Invariably, We Have Learned

At this point, we've examined four invariants and how their absence results in Violation Witness errors. 
This demonstrates how important it is to understand your program before it is time to write `{!rsh} invariant`s.
You should be prepared to create `{!rsh} invariant`s as soon as you need a `{!rsh} parallelReduce`.

Learn to quickly identify where your application needs a `{!rsh} parallelReduce` and what `{!rsh} invariant`(s) will be required to correctly `{!rsh} assert` the balance(s) and condition(s).
For more practice, continue to find examples, remove the invariants, and study the compile errors. 

If you'd like to experiment with invariants in additional examples then I recommend starting with the [NFT-Auction-API](https://github.com/reach-sh/reach-lang/blob/master/examples/nft-auction-api/index.rsh) and the [Chicken-Parallel](https://github.com/reach-sh/reach-lang/blob/master/examples/chicken-parallel/index.rsh) examples. 

This can be a good way of developing a sense of when and how to use invariants. 
Common error messages you may see are "balance zero at application exit" and "balance sufficient for transfer". 
If you see these errors in your applications, take time to consider the formula of the balance in your application. 
Seek to identify a mathematical relationship between loop conditions and the balance.

You should now have a better understanding of how to write invariants and their importance in developing secure Reach DApps.
If you need additional advice for your DApp, reach out in our [Discord](@{DISCORD}).