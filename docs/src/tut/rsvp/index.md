# {#tut-rsvp} Répondez S'il Vous Plaît

This tutorial walks through the creation of a simple, but real, decentralized application.
It assumes a basic familiarity with Reach, as though you've completed the @{seclink("tut")} tutorial, but does not dwell on intimate details of it.
Similarly, it assumes that you [have Reach installed](##quickstart) and are comfortable using it.

## {#tut-rsvp1} Respond, If You Please

In this tutorial, we'll be building a basic DApp for running events, which we'll refer to as "RSVP".
In this DApp, a _Host_ will run an event, like a dinner party, which _Guests_ will attend.
The Host wants the Guests to tell her if they are coming, so she accepts them to visit the DApp beforehand.
The Host does not want to charge the Guests for the privilege of their company, but she is nervous that she'll order too many macaroons.
Therefore, she requires that the Guests pay something when they agree to come.
We call this the "reservation".
But, when the Guests actually arrive and check-in, they will get their reservation back.

For example, this is a plausible run of the application:
```
Buffy decides to host "Buffy's Birthday Bash at the Bronze".
Xander, Willow, Cordelia, Giles, and Oz all agree to come.
The night of the party comes.
Angel tries to say he'll come, but it's too late.
Xander and Willow show up early.
Giles doesn't come because he's busy reshelving.
Oz is indisposed and can't make it.
Cordelia shows up late, but does come.
Jonathan tries to come in, but didn't let Buffy know, so he's turned away.
Buffy closes the event and takes Giles & Oz's reservations
```

If everyone has $10 before the event, and the reservation is $1, then afterwards:
```
Buffy has $12, because she took Giles & Oz's reservations.
Angel has $10, because his reservation failed.
Xander has $10, because he showed up.
Willow has $10, because she showed up.
Cordelia has $10, because she showed up.
Giles has $9, because he didn't.
Oz has $9, because he didn't.
Jonathan has $10, because he never made a reservation.
```

:::note
When you take this tutorial and launch your own product based on it, you should let Giles send his regrets if it is early enough!
Similarly, it would be cool if you made it so that the extra $2 was split between the Guests and the Host.
But, we won't implement these features today.
:::

When you build DApps, it is essential that you think through these kinds of scenarios first, before you start writing code, because they will help you know where to start and know when you are done.

## {#tut-rsvp2} Participate, Please

If you have done a prior Reach tutorial, like @{seclink("tut")}, then you are probably wondering what the participants in this program are.
1. Should we have a `Host` participant and a `Guest` participant?
1. Should we have a `Host` participant, as well as the participants `Guest1`, `Guest2`, up through `Guest5`?

If we do the first, then how will we have five guests, like in Buffy's example?
If we do the second, then what if Jenny decides to come? Would we need a totally different program?

The second idea is really bad and we should absolutely not do anything like unto it.
The first idea is not always dumb, but it turns out we're going to do something else for this application.
However, since it is sometimes a good idea, let's elaborate on what it means and why it can be valuable.

## {#tut-rsvp3} Yo Dawg...

If we had a Reach application with just two participants, one for the Host and one for the Guest, then if we had many Guests, we would need to have many different instances of the application.
This is similar to the @{seclink("tut")} application, where each instance plays exactly one game, but you can run the application as many times as you want.

For our event reservation application, each instance of the application would handle the registration of one Guest and we would build a user interface that supports launching many instances and (for the Host) managing each instance.
This is a "decentralized" decentralized application (a "De-DApp"), because there are many instances of the same application with a common user interface.
Sometimes people refer to "De-DApp"s as "protocols", because they are trying to emphasize the idea that there are many instances which all speak a common language.

In contrast, there are "centralized" decentralized applications ("Ce-DApps") where there is one single instance that manages all interactions within the application.

Some features are easier to build into De-DApps and others are more difficult, but are easier with Ce-DApps.
For example, we speculate that it is easier to add the feature that Guests who show up can split the abandoned reservations in a Ce-DApp than a De-DApp.

This concept of Ce-DApps and De-DApps is artistic and humanistic: there is no definitive technical definition.
In particular, there is a spectrum of centralization.
For example, just as we can imagine one application for a single Event but for all Guests, we can imagine one application for all Events and all Guests of that Event.
In that sense, we've just spoken about "De-De-DApps" ([decentralized-decentralized-decentralized applications](https://imgflip.com/i/6q5g9u)) and "De-Ce-DApps" as opposed to "Ce-Ce-DApps".

We are going to demonstrate the "decentralized" style, because it introduces fewer new Reach concepts, but we believe that the "centralized" design is more appropriate for this application.

## {#tut-rsvp4-tests} Test First, but Verify

But, rather than jumping into the Reach program, we're going to put our money where our mouth is and write a test scenario corresponding to Buffy's Birthday Bash.
We'll demonstrate how to use Reach's testing tools to write a convenient testing framework customized for your application.
We'll show the tests, then the framework, then the Reach code, and show how the Reach code connects to the framework.
This structure is overly complex for this simple application, but it is under complex for what it takes to build a "real" DApp.

---

We're going to use this exact same testing for each of the different versions of the application we build today, but each version will have a slightly different "framework" that connects totally different Reach code.

Let's get started:

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 83-89
```

In this sample, we use `{!js} test.one` to define a single test scenario.
We use the function `{!js} makeRSVP`, which we will define later, to create a JavaScript object for the Event abstraction.
When it is created, it has the details of the event in it.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 90-94
```

Next, we define objects for each of the people involved in the scenario.
This code uses `{!js} Event.makeGuests`, a function which we will define later, to turn a list of labels into Guest abstractions.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 95-96
```

Next, we have each one of the Scoobies declare that they will go to the event, and therefore pay the reservation.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 97-108
```

Next, we wait for the deadline and have people start showing up, or not.
In the case of Angel, when he says he'll go, there's an error, because he's late.
`{!js} err` is a constant we'll define later that is the text of the error message from the consensus network indicating that there was a problem submitting the transaction.
Similarly, in the case of Jonathan, when he tries to show up, there's an error.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 110-116
```

Finally, we print out the balances of everyone and see that they match our expectations.
The function `{!js} test.run` instructs Reach to run all of the tests and not print out extra debugging information.

Here's what an example run looks like on Algorand:
```
[... a lot of boilerplate about building images and running them ...]
Cordelia made reservation: 4828
Xander made reservation: 4827
Giles made reservation: 4830
Oz made reservation: 4829
Willow made reservation: 4831
Waiting until 4535
Checking in Xander to 4827...
Xander did show.
Checking in Willow to 4831...
Willow did show.
Checking in Cordelia to 4828...
Cordelia did show.
Checking in Giles to 4830...
Giles did not show.
Checking in Oz to 4829...
Oz did not show.
Buffy has 101.9853 ALGO
Xander has 99.9962 ALGO
Willow has 99.9962 ALGO
Cordelia has 99.9962 ALGO
Giles has 98.9962 ALGO
Oz has 98.9962 ALGO
Angel has 99.9993 ALGO
Jonathan has 100.0003 ALGO
```
This sample contains extra printouts we didn't show, because they are embedded in the "framework" (i.e. `{!js} makeRSVP`) that we'll show later.

Ideally, we would make real tests that assert that these numbers are correct, but in practice it is very difficult to do so, because of protocol fees and (on networks like Algorand) rewards.
One work around for this is to use deliberately huge numbers (like a reservation of 50) and have tests that do "epsilon comparison" (i.e. rather than writing `{!js} x == y`, you write `{!js} (x - y) <= epsilon`, where `epsilon` is a small number).
We won't bother with that today, however.

## {#tut-rsvp4-rsh} A Decentralized DApp

Now that we've shown the test case that we'll use for every version, let's look at the "decentralized" version of this DApp.

Recall, that in the "decentralized" version, there will be one instance of the application for every Guest that goes to the Event.
Since the Host can't know how many Guests there are going to be, she cannot create all of the instances.
So, in our design, the Guest will create the instance themselves.
When the Guest checks in, they have to tell the Host about the instance so the Host can interact with it and return the reservation.
The Host will have to check to make sure the instance is for the correct Event.
On the other hand, if the Guest doesn't check in, then the Host has to find out about the instance somehow and claim the reservation.
(This "somehow" is why this may not be the best design for this application.)

```
load: /examples/rsvp-4-tedede/index.rsh
md5: fbd2ccb5df12a9467d5214f1e84dbf0c
range: 1-20
```

First, we define a type that represents the details of the event.
It contains the name of the event ("Buffy's Birthday Bash..."), as well as the reservation price, the deadline, and the host's address.

Second, we define the application and the two participants, the Host, and the Guest.

```
load: /examples/rsvp-4-tedede/index.rsh
md5: fbd2ccb5df12a9467d5214f1e84dbf0c
range: 22-29
```

As mentioned above, the first thing that happens in the program is that the Guest launches the instance by doing the first publication.
They publish the details of the event, which they had to get from the Host somehow.
(Another "somehow"!)
And they have to pay the reservation fee.

We enforce that the current time on the network is before the deadline.
This is not strictly necessary, but it will be convenient and helpful for end users.

:::note
What is `{!rsh} enforce`?

It is similar to `{!rsh} require`, except that Reach does not guarantee that there is an `{!rsh} assume` that dominates it.
This means that honest actors may submit publications that will not be satisfied, because they violate the property.
You should use this sparingly and only for properties that cannot be checked locally, like things related to time (as in this case) or things related to external `{!rsh} remote` objects.
:::


Finally, the Guest is informed, via their `registered` `{!rsh} interact` function, about the contract's identity, so they can share it with the Host.

:::note
Did you notice that the `{!rsh} interact` function was called outside of an `{!rsh} only` block and within a consensus step?

When an `{!rsh} interact` function returns `{!rsh} Null`, it can be called via the short-hand `{!rsh} Participant.interact.f(....)`.
This a short-hand for writing `{!rsh} Participant.only(() => interact.f(....))`.

When a local step (such as the `{!rsh} interact` short-hand) appears in a consensus step, it will run after the consensus step is confirmed, whenever the participant learns about that confirmation.
:::

```
load: /examples/rsvp-4-tedede/index.rsh
md5: fbd2ccb5df12a9467d5214f1e84dbf0c
range: 31-44
```

It is now time for the Host to show up and declare what should happen to the reservation.
First, the Host checks to see if the details are correct.
If they are, then the Host publishes a boolean for whether the Guest really showed up or not.
When they publish, it is checked locally and consensually whether the sender is the specified Host.
We ensure that the time is after the deadline, and then send the reservation to the appropriate place.

:::note
What is `{!rsh} check`?

It is like `{!rsh} assume` and `{!rsh} require`, except that it morphs into whichever is appropriate given the context of its use.
This is convenient for writing functions that validate input data that can be used locally (like `{!rsh} assume`) _and_ in consensus (like `{!rsh} require`).
:::

:::note
How does the Guest know that the Host is going to be honest and report that they attended accurately?

There is no way for the Guest to enforce this.
They must trust the Host to do this correctly, just like they need to trust that the Host is not going to throw a lame party.
This is an example of the so-called "oracle problem" in consensus networks: when you want to represent on-chain information that is only available off-chain, you must have some trusted party that can bridge the information from the "real" world into the consensus network.
In this case, the Host is the Oracle.
:::

---

This application works well and we'll be able to run our test scenario.
It is extremely simple, as a Reach program, but its main downside is that it requires complexity outside of the consensus network.

For example, how does the Host inform Guests what the proper event details are?
We assume that the Host is going to have a Web page that shows these events in a machine-readable format, like a QR code, which the RSVP application will consume and automatically use as the `{!rsh} details` variable.
One precise way this could work is that the Host goes to the RSVP application site, enters the information, and gets a custom link where this information is embedded in the URL, like `https://rsvp.app/event?d=SXQncyBhIHNlY3JldCB0byBldmVyeW9uZQo=`, and when they share the link, the application will be in "Guest mode".
The link could also contain things that are useful outside of the core consensus program, like a link to a banner image and other important details.
This would be a completely stateless way to implement the application.

Similarly, how do the Guests inform the Host about their instance of the program?
One strategy for this is that the RSVP application generates a QR code which is an encoded form of their `{!rsh} getContract` value.
When the Guest arrives at the event, they show the QR code, which the Host scans with the RSVP application in "Host mode" and then it runs the next step of the program.

But, how does the Host find out about Guests that reserved, but never came?
In one way, it doesn't matter: the Host doesn't want the money, she just wants the Guest to have an incentive to come.
That answer is very unsatisfying though.

There are roughly two alternatives:
- First, the Guest could tell the Host up-front that they are coming, but they would have to do that off-chain, such as by the RSVP application tracking it in a centralized database or encouraging the Guest to send an automated email or SMS message.
- Second, the Host could search the consensus network for all instances of the RSVP application with matching details and then deliberately interact with and claim the funds.

The first option is "centralized", while the second option is "decentralized".
(It appears, we now have a "decentralized, decentralized, decentralized, decentralized application" design, if we go with the second option.)

Both of these options are tedious and annoying, so we will eventually use a different technique to engineer the RSVP application.
But before we do that, let's look at the JavaScript framework that makes our test scenario work.
It makes explicit these issues about who (Guest or Host) does what, when, and how.

## {#tut-rsvp4-mjs} My Kingdom for a Framework!

In @{seclink("tut-rsvp4-tests")}, we showed a set of tests which we will use for each instance of the RSVP application code.
In @{seclink("tut-rsvp4-rsh")}, we showed the Reach program that implements an extremely decentralized version of this program.
In this section, we show the testing framework that connects these two things.

This framework needs to provide:
- `{!js} makeRSVP // :: Details -> Event` --- A function which accepts the details of an event and returns an Event abstraction.
- `{!js} Event.Host // :: Person` --- An abstraction of the Host.
- `{!js} Event.makeGuests // :: [string] -> Promise<[Guest]>` --- A function that produces an array of Guest abstractions, which are subclasses of People abstractions.
- `{!js} Guest.willGo // :: -> Promise<void>` --- A function for one guest to make a reservation.
- `{!js} Event.waitUntilDeadline // :: -> Promise<void>` --- A function that waits until the deadline has passed.
- `{!js} Guest.showUp // :: -> Promise<void>` --- A function for one guest to be checked in.
- `{!js} Guest.noShow // :: -> Promise<void>` --- A function for one guest to be indicated as failing to check in.
- `{!js} Person.getBalance // :: -> Promise<UInt>` --- A function to read one person's balance.

We'll talk about each of these in turn.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 1-10
```

First, we have the basic header that imports and initializes the Reach standard library.
The only interesting thing here is the definition of the various error messages for each network connector.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 13-16
```

We define the `{!js} makeRSVP` function and create an initial test account for the host and set its label for debugging.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 18-33
```

Next, we define the function `stdPerson` which takes an `obj` with an `acc` field and adds a `{!js} Person.getBalance` function that returns the account's current balance as a nice formatted string.
We use this function to define the `{!js} Event.Host` value.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 33-39
```

Next, we define the deadline, based on the current time, and the `{!js} Event.waitUntilDeadline` function for waiting until that time has passed.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 41-43
```

Now, we can define the `details` object that will be consumed by Reach.
This value will be in scope for each of the Guests, which represents the Event details being shared with all the Guests.
We use `accHost` for the `host` field, because Reach can convert account abstractions into `{!rsh} Address` values.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 45-47
```

We define the `{!js} Event.makeGuest` function, which starts by creating a new test account and setting its label.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 49-57
```

We define the `{!js} Guest.willGo` function, which spawns a new contract that runs as the `Guest` participant.
It uses `{!js} stdlib.withDisconnect` and `{!js} stdlib.disconnect` to stop running after the `registered` `{!rsh} interact` function has been called with the contract information.
This information is returned from `{!js} stdlib.withDisconnect`, so `ctcInfo` is set to the actual contract that was deployed.
In a real deployment, this contract information would be presented the user as something like a QR code for them to save and show to the Host later.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 58-70
```

We define the two functions `{!js} Guest.showUp` and `{!js} Guest.noShow` by defining a common function `doHost` that accepts a boolean.
This function throws an error if there was never a reservation, which we know because the `ctcInfo` variable is undefined.
Although the function is inside of the Guest object, it uses the Host's account abstraction to attach to an instance of the program.
It has access to `details` object, so it can ensure that they match.

```
load: /examples/rsvp-4-tedede/index.mjs
md5: abe189d9c995bd717c19301dee5706d9
range: 72-80
```

We close the definition of the Guest abstraction by calling `stdPerson` to add the `{!js} Person.getBalance` function.
Then, we define `{!js} Event.makeGuests`, which produces a single promise out of the array of promises of Guest abstractions.
These values are all wrapped together into a final object, which is the result of `{!js} makeRSVP`.

---

At this point, we have a completely working DApp and testing framework.
But, turning this into a viable product and usable experience is more difficult.

Rather than do that, we're going to switch gears and show a different way to write this application that is a _centralized_ DApp.

## {#tut-rsvp5-api} The API and The Map

Recall that our previous version uses one instance for every Guest that goes to an Event and that this is tedious for the Host to communicate to the Guests what the Event details are and difficult for the Guests to communicate to the Host that they are intending to come and for the Host to note whether the Guests actually came.

The key source of this tediousness is its over decentralization: the information is spread throughout the system and duplicated many times.
In some ways, this kind of decentralization is very valuable, because it means that there is not a single-point-of-failure, like how your body has many many different immune cells rather than a single giant immune cell.
However, that benefit is unnecessary when we are already running on a consensus network, where decentralization at the base layer provides trustlessness at the application layer.
Indeed, the difficulty of the over decentralized model leads some designer to build an off-chain centralized database, like a database of the active events, because it is so difficult to query the consensus network for matching application instances.
Those centralized databases become a trusted point-of-failure that are worthy targets of attack and exploitation.

Instead, we're going to write a version of RSVP using a single instance.

If you're only familiar with Reach from the @{seclink("tut")} tutorial, then you should be wondering, "But how are you going to let any number of Guests join? And where will you store their information?"
We're going to use two new concepts to implement this:
1. APIs --- which are a source of publications that do not originate in a participant are a source of asynchronous input events to the consensus network.
1. Mappings --- which are a database of values that grow linearly over the lifetime of the program.

We will define an API that Guests can call to register for an Event and we'll define another API that the Host can call whenever they want to report whether a Guest showed up.

We will define a Mapping that records whether a Guest has registered and we will clear that record when the Host reports their attendance.

Since API publications do not come from participants, but could come from any number of actors in the consensus network, including other contracts, AND because Mappings can grow linearly in the number of actors that interact with the program, there is no limit to the number of Guests!

The overall structure of our application will be:
1. The Host creates an application instance.
1. Guests repeatedly make reservations, before the deadline.
1. The Host repeatedly reports whether Guests come.
1. The program ends when all reserved Guests are accounted for.

## {#tut-rsvp5-rsh} Programming with APIs and Maps

Let's dig into the code!

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 1-9
```

We define an object for the details of the Event, which is the same as before.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 11-22
```

We define the participant and then the two APIs.
- The participant, `Admin`, is the one that actually creates the instance.
  In most cases, this will be the Host, but we don't restrict it so it has to be the same person.
- The first API, `Guest`, is for Guests to call and it has just a single function.
  On-chain, this function will be callable as `Guest_register`, according to whatever the ABI standard is for the chain.
  Off-chain, Reach provides an interface in its standard library to call it as `{!js} ctc.apis.Guest.register()`.
- The second API, `Host` is for the Host to call as they check-in (or note the failure to show) of Guests.
  It takes two arguments: the first for who the Guest is and the second for whether they showed up, or not.
  It can be called on-chain as `Host_checkin` and off-chain with `{!js} ctc.apis.Host.checkin(guest, showed)`.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 24-30
```

We publish the details, unpack them, and signal to the creator that the contract has been launched.
However, we do _not_ `{!rsh} commit()`, because we are going to do something else in the consensus.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 32-35
```

This is a very dense code sample with lots of new ideas if you're only familiar with basic Reach.
It is worth studying closely, because these ideas are used over and over in complex Reach programs.

First, we define a new `{!rsh} Map` that stores a boolean value for each address used as a key.
We're going to use this to store the set of Guest accounts.
(There is a `{!rsh} Set` container available too that abstracts this pattern, but we're going to show the "raw" version to explain mappings in detail.)

We can set entries in the mapping by writing `{!rsh} Guests[addr] = bool` (or `{!rsh} delete Guests[addr]` to remove them).
We will be able to access entries of the mapping by writing `{!rsh} Guests[addr]`, which will evaluate to a `{!rsh} Maybe(Bool)` value.
It will be `{!rsh} None()` if the value is not set.
It will be `{!rsh} Some(bool)` if the value is set.

:::note
Some people find `{!rsh} Maybe`s to be tedious, but they are an important protection against NULL pointer dereferences that you see in other systems.

You may enjoy reading Sir Tony Hoare's [explanation](https://en.wikipedia.org/wiki/Null_pointer#History) about how and why he created NULL pointers and why he has regretted it ever since.
We think his cost estimate of the damage of NULL pointers at one billion dollars is too low by an order of magnitude.
:::

Second, we start a "parallel reduce" block.
In computer science, a [reduction](https://en.wikipedia.org/wiki/Reduction_operator) is when a set of data is turned into a single value; i.e. it is _reduced_ to one value.
For example, if you have a set of numbers, and you add them together, to get the sum, that's a reduction.
In this case, we are reducing a set of input events (API calls) that occur in _parallel_ to each other (meaning that they are independently chosen by their initiators) into two values, `done` and `howMany`.
`done` is going to be a boolean that tells us if the Event is over and we can clear it from memory; it starts as `{!rsh} false`.
`howMany` is a running counter of how many Guests have registered, but have not checked in; it starts as `{!rsh} 0`.
Shortly, we'll review the code that actually reduces incoming events into updated versions of these values.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 34-37
```

Every `{!rsh} parallelReduce` can be written as a `{!rsh} while` loop, but it is often more convenient to think about it as a separate kind of construct.
Because it is a `{!rsh} while` loop, it must have a condition for when it terminates (a `{!rsh} .while` component) and loop invariants (a sequence of `{!rsh} .invariant` components).
In our case, we have two invariants and one condition.
- The first invariant is that the variable `howMany` is the same as the size of the `Guests` mapping.
- The second invariant is that the contract's balance is the same as `howMany` times the number of reservations; in other words, that we can perfectly predict how much funds are in the contract's account.
- The condition is that we will continue accepting (and reducing!) input events until
  we are done _and_ `howMany` is zero; because only then is it safe to turn off the ability to check in Guests.

We can now show the reduction blocks for each different kind of input event.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 38-47
```

This uses a new form you've never seen before: the `{!rsh} .api_` component.
It has two arguments:
1. First, there's the API call that is actually being handled; in this case, the `{!rsh} Guest.register` call.
1. Second, there's a function that accepts the arguments to call (in this case there are none) and specifies the action.

The action specification function is made of two parts:
1. First, there's a sequence of `{!rsh} check`s that validate whether the API can be validly called.
  In this case, we check to ensure that the Event hasn't started and that the Guest has not registered (by using the function `{!rsh} isNone` to test the contents of the mapping).
  Although we don't in this example, you can define values here that are used later in the reduction handler.
1. Second, there's a `{!rsh} return` that specifies (a) what should be paid when this call is made, and (b) what happens in the consensus when it is called, specified as a function.
  In this case, we have to pay the reservation price.

The consensus reduction specification function accepts an argument (traditionally labeled `ret` (for "return") or `k` (for "continuation")) that must be called with the API call result.
The function can then perform any additional checks or effects, before yielding the result (by invoking `ret`) and then returning updated values for the reduction.

In this case, the function
1. ensures that it is not too late;
1. stores that the Guest is coming in the mapping;
1. yield `{!rsh} null` to the API caller; and,
1. increments the count of the number of Guests, `howMany`.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 48-58
```

The Host checkin function is similar.
It accepts the Guest's address and whether the Host is reporting that they showed up or not.
It ensures that the initiator of the API call is the Host.
It ensures that the named Guest actually made a reservation.
It does not make the Host pay anything.
It ensures that the Host is doing this after the deadline.
It removes the Guest's entry from the mapping.
It transfers the reservation fee to either the Guest or the Host, depending on the value of `showed`.
And, finally, it decrements the Guest counter and indicates that Event has started.

```
load: /examples/rsvp-5-cede/index.rsh
md5: 210135131e027cf541a3cf64f3b88ce9
range: 59-62
```

Finally we close the applications.
We don't need to do any final `{!rsh} transfer`s or anything like that to satisfy the token linearity property (which enforces that the contract's account is empty when it ends), because we know that we only reach the end of the `{!rsh} parallelReduce` when `howMany` is zero, and we know that the contract balance is `howMany * reservation`, so it must be zero in that case too.

---

Although we did not dwell on the verification of this program, nearly every line in the program is essential in some way and many of them are essential for passing the verification engine.

```
load: /examples/rsvp-5-cede/index-fail.rsh
md5: 5e7a31fdaec3a38cbd4a37a896375fbc
range: 48-58
```

For example, suppose that we neglected to delete the Guest's reservation entry.
This means that we will decrement `howMany` without changing the size of the mapping, so one of the loop invariants will be incorrect.
Reach will detect this error and show the following error during verification:

```
load: /examples/rsvp-5-cede/index-fail.txt
md5: f07e7976fec1b2c377baaaaa8c033f17
range: 5-26
```

This program is interesting, because the invariants and assertions are set up in such a way that almost no other program could possibly satisfy them.
In general, this is a good property to seek when you're writing trust-worthy software: you want any change to be detected by your verification engine and test suite.

## {#tut-rsvp5-mjs} Testing our APIs and Maps

We've just walked through the Reach implementation of the RSVP application using APIs and Maps.
But, now we need to show the updates to our testing framework that work with the new implementation.

The program starts exactly the same as before:

```
load: /examples/rsvp-5-cede/index.mjs
md5: 2721732986c261f7ca435a92246fc621
range: 1-45
```

But after we define the details, there's a difference:

```
load: /examples/rsvp-5-cede/index.mjs
md5: 2721732986c261f7ca435a92246fc621
range: 47-52
```

We define the Host's contract handle and have them run the Admin participant, which creates and launches the single contract instance for everyone.
As mentioned above, we could have a separate account do this, but we choose not to for simplicity.

```
load: /examples/rsvp-5-cede/index.mjs
md5: 2721732986c261f7ca435a92246fc621
range: 54-70
```

The code for guests is much simpler, because in each case we just have either the Guest call the `register` function or the Host call the `checkin` function.

```
load: /examples/rsvp-5-cede/index.mjs
md5: 2721732986c261f7ca435a92246fc621
range: 72-80
```

Everything else about the program is the same.

---

When we run the program, we will see output like:
```
[... a lot of boilerplate about building images and running them ...]
Buffy launched contract
Giles made reservation
Xander made reservation
Cordelia made reservation
Willow made reservation
Oz made reservation
Waiting until 5669
Checking in Xander...
Xander did show.
Checking in Willow...
Willow did show.
Checking in Cordelia...
Cordelia did show.
Checking in Giles...
Giles did not show.
Checking in Oz...
Oz did not show.
Buffy has 101.9853 ALGO
Xander has 99.9962 ALGO
Willow has 99.9962 ALGO
Cordelia has 99.9962 ALGO
Giles has 98.9962 ALGO
Oz has 98.9962 ALGO
Angel has 99.9993 ALGO
Jonathan has 100.0003 ALGO
```
This is output is effectively identical to the previous version of the program.

---

This program is excellent and we wouldn't need to make any fundamentally different decisions about its design if we were to really launch the `rsvp.app` service.
However, there are a few things that are difficult to do with the program as it is.

For example, ideally when you go to `rsvp.app/event?id=bbbb`, you should be able to see some information about the Event, like who the Host is, what's the reservation price, and so on.
This information is embedded in the consensus network's records and in the state of the contract instance, but it requires low-level knowledge to extract.
We're going to add a `{!rsh} View` to the program to make it easy to access.

Similarly, we'd like to display information about how many people have already made reservations and, maybe, even who they are and when it happened.
For the first of those things, we'll add another `{!rsh} View`; and for the second, we'll add an `{!rsh} Events` that will make it easy to access the record of everything that happened.

:::note
Although we're going to make changes to the Reach program, we're not actually exposing any information that wasn't already available in the consensus network's records.

For example, the `{!rsh} View` is going to make it easy to read the `details` and `howMany` variables.
These variables are actually stored in the consensus network's memory.
It is possible, for example, to know that "`howMany` is stored at memory offset 0x8008" and that is encoded as 64 bytes in little-endian order.
However, those details are extremely low-level and subject to change as the consensus network's implementation improves and Reach discovers more efficient ways to compile your program.

By adding a `{!rsh} View`, we'll produce a usable API that abstracts these kinds of details for the benefit of our colleagues that create the User Interface, our future selves that need to understand the program, and our users that want to have easy access to data about themselves.
:::

## {#tut-rsvp6-rsh} A View To An Event

First, we'll review the changes to the Reach application code.

```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 11-31
```

We add definitions for the `{!rsh} View` and `{!rsh} Event` objects.

Let's look at the `{!rsh} View` first.
The first argument is a label for it, like how we give labels to APIs and participants.
Next, we provide an object where the keys are the names of the view components and the fields are their types.
This object is just like an `{!rsh} interact` object, except that the values are provided _from_ Reach, rather than _to_ Reach.
In this case, like APIs, these values can be accessed on- and off-chain.
On-chain, they can be accessed using the normal ABI of the consensus network, just like APIs.
For example, the details are provided via a function named `Info_details` that takes no arguments and returns a `Details` structure, and there's a function named `Info_reserved` that accepts an address and returns a boolean indicating if they've made a reservation.
Off-chain, they can be accessed via a frontend function like `{!js} ctc.views.Info.details()`.
The off-chain function returns the value or an indication that it was not available.

Next, let's look at the `{!rsh} Events` definition.
It can also be provided with a label, but we've chosen not to include one.
We don't have to provide labels for `{!rsh} API`s or `{!rsh} View`s either, but we think it is a good idea in those cases.
The object provided to `{!rsh} Events` is not an interface, where the keys are types, but instead has tuples of types as the values.
These are the values that will be emitted together.
For example, the `register` event will contain one address, while the `checkin` event will contain an address and a boolean.
Like APIs and Views, they are available on- and off-chain.
On-chain, they are available using the standard ABI for the platform.
(Although, note, that some chains, like Ethereum, don't provide any on-chain mechanism for consuming events.)
Off-chain, they are available via a frontend function like `{!js} ctc.events.register`.
The off-chain function has sub-methods for reading the next instance of the event or monitoring every event, as well as other options.

In both cases, we have not actually defined the values or meaning of these Views and Events.
We've merely indicated that our application contains them.
This is similar to how we define a Participant and then later indicate what actions it performs.
Let's look at the view definitions next.

```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 33-40
```

A View can have a different value at each point in the program, including not having any value at all.
You define the value by calling `{!rsh} View.field.set` and providing a value that satisfies the type.
For example, here (on line 40) we indicate that the `details` field is always the same as the `details` variable.
This definition applies to all dominated occurrences of the `{!rsh} commit()` keyword.
Views are not mutable references: instead, they are ways of naming, for external consumption, portions of the consensus state.

```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 42-51
```

We similarly expose the contents of the `Guests` mapping, as well as the `howMany` variable.
We use the `{!rsh} .define` feature of `{!rsh} parllelReduce` to introduce a statement that dominates the `{!rsh} commit()`s implicit in the `{!rsh} parallelReduce`.
This context is the only context that has access to the `howMany` variable, which is why we must place it there.

Next, let's look at the code that emits instances of the `{!rsh} Events` we defined.

```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 52-62
```

We can emit an event by calling `{!rsh} Events.kind(args)` in a consensus step.
We do so inside of the `{!rsh} .api_` for the `Guest.register` API call on line 58.

```
load: /examples/rsvp-6-vevt/index.rsh
md5: ed7d96413f2f23224d5ea6081ae4cc78
range: 63-74
```

We do the same for the `Host.checkin` event on line 70.
It is typical to emit events just before or after yield a result to the API caller.

## {#tut-rsvp6-mjs} The Panopticon

We've now exposed information about the state and history of the RSVP application to callers.
If we were going to build a real version of this application, we'd include calls to the `{!rsh} View` functions in the user interface and provide an event logger that monitors the `{!rsh} Events`.
However, we'll delay that for another time.
Instead, we'll just make a few simple changes to the test framework to demonstrate how to use these functions.

```
load: /examples/rsvp-6-vevt/index.mjs
md5: 65e69ecdbcea0eab0d04838833ddea55
range: 47-57
```

First, we'll have the Host monitor all registrations for this event.
`{!js} ctcHost.events.register.monitor(f)` is a function that calls `f` once for every event.
`f` is called with an object that has a `when` field and a `what` field.
`when` is the time when the event was emitted from the consensus network.
`what` is an array of the values that were included with the event.
In this code, we extract those fields and print out a message on every registration.
This is representative of a user interface that shows the Host each registration as it happens.

```
load: /examples/rsvp-6-vevt/index.mjs
md5: 65e69ecdbcea0eab0d04838833ddea55
range: 59-70
```

We've added a gas limit using `{!js} stdlib.setGasLimit` for testing on Ethereum.
Normally the gas limit is computed by looking at the contract code, but it is not always accurate.
There is always a gas limit, even if we don't specify anything.
We've set the "limit" to the largest possible amount of gas that could ever be spent.
Thus, we're removing the limit by setting it to be "infinity".

Next, we modify the `willGo` function so that the Guest, before they register, inspects the event details and looks at the reservation price.
This is representative of a user interface that informs the Guest of how much they'll be expected to pay to make a reservation.
In this code, we call `{!js} ctcGuest.unsafeViews`, because when the view is not defined, it throws an error, rather than returning a special wrapper object.
We know that it will always be defined, so it is more convenient to use this, than to worry about decoding the wrapper object.

---

When we run this version of the program, it behaves exactly the same as before in all important ways, but has a slightly different output:

```
[... a lot of boilerplate about building images and running them ...]
Buffy launched contract
Xander sees event reservation is 1 ALGO
Cordelia sees event reservation is 1 ALGO
Willow sees event reservation is 1 ALGO
Oz sees event reservation is 1 ALGO
Giles sees event reservation is 1 ALGO
Giles made reservation
Xander made reservation
Cordelia made reservation
Willow made reservation
Oz made reservation
Waiting until 5669
Angel sees event reservation is 1 ALGO
Buffy sees that S5YGHPLCBNX3KWS7U2MPEXQOQJ6H3HTFTAPLAPNFSHXZGKKPMRLLUHJ2SU registered at 5661
Buffy sees that 7CU32PBE3LFQOBV53BIZZ4BRRKUGJRCKZEKRP5IT3FNXAYIBB6GJSQHGL4 registered at 5662
Buffy sees that 72MKLYQN56GKZOCGIZHTJ6SQCEDBSRAMCLDPQAGZTZTB6LX6RG4RBR5WGY registered at 5663
Buffy sees that WEQZOPCRY7VMA5JEJMZPTHYJFFSQIAXO4FJX23RPY3QXV3I53KDGMVBIPQ registered at 5664
Buffy sees that LMGM4ZA24RTJTV6BCDWHKBMGG533XBVOH4MSTLHMUHYGLCEYOINY6BO5NI registered at 5665
Checking in Xander...
Xander did show.
Checking in Willow...
Willow did show.
Checking in Cordelia...
Cordelia did show.
Checking in Giles...
Giles did not show.
Checking in Oz...
Oz did not show.
Buffy has 101.9853 ALGO
Xander has 99.9962 ALGO
Willow has 99.9962 ALGO
Cordelia has 99.9962 ALGO
Giles has 98.9962 ALGO
Oz has 98.9962 ALGO
Angel has 99.9993 ALGO
Jonathan has 100.0003 ALGO
```

## {#tut-rsvp7} Where Do We Go From Here?

We've implemented a full DApp for managing events RSVPs in less than 80 lines of Reach code.
In less than a hundred lines of JavaScript, we have a testing system where we can implement test scenarios to ensure it is correct.
This is a lot to do in an afternoon, but it is better than writing the 3,000 lines of Solidity, TEAL, and JavaScript support code that Reach generated for us!

You should now be asking, [Where Do We Go From Here?](https://www.youtube.com/watch?v=7XdAQpq_1Xw)
You now understand...
- How to design decentralized and centralized DApps;
- How to use `{!rsh} API`s in Reach programs;
- How to use `{!rsh} Map`s in Reach programs;
- How to define `{!rsh} View`s, for observing Reach programs internal state;
- How to define `{!rsh} Events`, for monitoring Reach program actions; and,
- How to do it all with a testing-first framework.

We find that many many users want to write DApps that are more like @{seclink("tut-rsvp")} than @{seclink("tut")}, so hopefully this tutorial will help you on your way!

If you want to extend this program and make it even better and more interesting, you should create a Web interface to it.
The interface will work by...
- Initially showing an "Admin" interface, where an administrator can launch an Event contract instance.
- Upon launch, it should turn the contract information into a special URL, or QR code, that can be shared with potential Guests and the Host.
- The Guest view should allow Guests to see the Event details and the current number of reservations.
  It should do this without requiring the user to attach a wallet, but if they want to make a reservation, they would have to.
  It should check to make sure they haven't already registered first (by using the `Info.reserved` view).
- The Host view should allow the Host to see who has registered and provide an interface for checking them in, either by clicking a button associated with each `Notify.register` event, or by scanning their address on their mobile wallet.

There is nothing new you'd need to learn about Reach to write that Web interface, but you'd have to be good at designing and building Web applications.
If you were using Algorand and wanted to provide a wallet to users who don't have an ARC11 wallet, then you'd use `{!js} stdlib.walletFallback`, but other than that, your code would look almost identical to our test suite, except you'd have lots of Web interface manipulation code.

So, remember, there's only one thing on this earth more powerful than evil, and that's us!
