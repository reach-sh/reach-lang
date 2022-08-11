# {#tut-rsvp} Répondez S'il Vous Plaît

This tutorial walks through the creation of a simple, but real, decentralized application.
It assumes a basic familiarity with Reach, as though you've gone @{seclink("tut")} tutorial, but does not dwell on intimate details of it.
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

In contrast, there are "centralized" decentralized application ("Ce-DApps") where there is one single instance that manages all interactions with the application.

Some features are easier to build into De-DApps and others are more difficult, but are easier with Ce-DApps.
For example, we speculate that it is easier to add the feature that Guests who show up can split the abandoned reservations in a Ce-DApp than a De-DApp.

This concept of Ce-DApps and De-DApps is artistic and humanistic: there is no definitive technical definition.
In particular, there is a spectrum of centralization.
For example, just as we can imagine one application for a single Event but for all Guests, we can imagine one application for all Events and all Guests of that Event.
In that sense, we've just spoken about "De-De-DApps" (decentralized-decentralized-decentralized applications) and "De-Ce-DApps" as opposed to "Ce-Ce-DApps".

We are going to demonstrate the "decentralized" style, because it introduces fewer new Reach concepts, but we believe that the "centralized" design is more appropriate for this application.

## {#tut-rsvp4-tests} Test First, but Verify

But, rather than jumping into the Reach program, we're going to put our money where our mouth is and write a test scenario corresponding to Buffy's Birthday Bash.
We'll demonstrate how to use Reach's testing tools to write a convenient testing framework customized for your application.
We'll show the tests, then the framework, then the Reach code, and show how the Reach code connects to the framework.
This structure is overly complex for this simple application, but it is under complex for what it takes to build a "real" DApp.

---

We're going to use this exact same testing for each of the different versions of the application we build today, but each version will have a slightly different "framework" that connects totally different Reach code.

Let's get started:

XXX sample one

In this sample, we use `{!js} test.one` to define a single test scenario.
We use the function `{!js} makeRSVP`, which we will define later, to create a JavaScript object for the Event abstraction.
When it is created, it has the details of the event in it.

XXX sample two

Next, we define objects for each of the people involved in the scenario.
This code uses `{!js} Event.makeGuests`, a function which we will define later, to turn a list of labels into Guest abstractions.

XXX sample three

Next, we have each one of the Scoobies declare that they will go to the event, and therefore pay the reservation.

XXX sample four

Next, we wait for the deadline and have people start showing up, or not.
In the case of Angel, when he says he'll go, there's an error, because he's late.
`{!js} err` is a constant we'll define later that is the text of the error message from the consensus network indicating that there was a problem submitting the transaction.
Similarly, in the case of Jonathan, when he tries to show up, there's an error.

XXX sample five

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
(This "somehow" is why this might not be the best design for this application.)

XXX sample one

First, we define a type that represents the details of the event.
It contains the name of the event ("Buffy's Birthday Bash..."), as well as the reservation price, the deadline, and the host's address.

Second, we define the application and the two participants, the Host, and the Guest.

XXX sample two

As mentioned above, the first thing that happens in the program is that the Guest launches the instance by doing the first publication.
They publish the details of the event, which they had to get from the Host somehow.
(Another "somehow"!)
And they have to pay the reservation fee.
We enforce that the current time on the network is before the deadline.
This is not strictly necessary, but it will be convenient and helpful for end users.
Finally, the Guest is informed, via their `registered` `{!rsh} interact` function, about the contract's identity, so they can share it with the Host.

:::note
Did you notice that the `{!rsh} interact` function was called outside of an `{!rsh} only` block and within a consensus step?

When an `{!rsh} interact` function returns `{!rsh} Null`, it can be called via the short-hand `{!rsh} Participant.interact.f(....)`.
This a short-hand for writing `{!rsh} Participant.only(() => interact.f(....))`.

When a local step (such as the `{!rsh} interact` short-hand) appears in a consensus step, it will run after the consensus step is confirmed, whenever the participant learns about that confirmation.
:::

XXX sample three

It is now time for the Host to show up and declare what should happen to the reservation.
First, the Host checks to see if the details are correct.
If they are, then the Host publishes a boolean for whether the Guest really showed up or not.
When they publish, it is checked locally and consensually whether the sender is the specified Host.
We ensure that the time is after the deadline, and then send the reservation to the appropriate place.

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
- Second, the Host could search the consensus network for all instances of the RSVP application with matching details and then deliberately interact with an claim the funds.
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

XXX sample one

First, we have the basic header that imports and initializes the Reach standard library.
The only interesting thing here is the definition of the various error messages for each network connector.

XXX sample two

We define the `{!js} makeRSVP` function and create an initial test account for the host and set its label for debugging.

XXX sample three

Next, we define the function `stdPerson` which takes an `obj` with an `acc` field and adds a `{!js} Person.getBalance` function that returns the account's current balance as a nice formatted string.
We use this function to define the `{!js} Event.Host` value.

XXX sample four

Next, we define the deadline, based on the current time, and the `{!js} Event.waitUntilDeadline` function for waiting until that time has passed.

XXX sample five

Now, we can define the `details` object that will be consumed by Reach.
This value will be in scope for each of the Guests, which represents the Event details being shared with all the Guests.
We use `accHost` for the `host` field, because Reach can convert account abstractions into `{!rsh} Address` values.

XXX sample five

We define the `{!js} Event.makeGuest` function, which starts by creating a new test account and setting its label.

XXX sample six

We define the `{!js} Guest.willGo` function, which spawns a new contract that runs as the `Guest` participant.
It uses `{!js} stdlib.withDisconnect` and `{!js} stdlib.disconnect` to stop running after the `registered` `{!rsh} interact` function has been called with the contract information.
This information is returned from `{!js} stdlib.withDisconnect`, so `ctcInfo` is set to the actual contract that was deployed.
In a real deployment, this contract information would be presented the user as something like a QR code for them to save and show to the Host later.

XXX sample seven

We define the two functions `{!js} Guest.showUp` and `{!js} Guest.noShow` by defining a common function `doHost` that accepts a boolean.
This function throws an error if there was never a reservation, which we know because the `ctcInfo` variable is undefined.
Although the function is inside of the Guest object, it uses the Host's account abstraction to attach to an instance of the program.
It has access to `details` object, so it can ensure that they match.

XXX sample eight

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
Indeed, the difficulty of the over decentralized model leads some designer to build off-chain centralized database, like a database of the active events, because it is so difficult to query the consensus network for matching application instances.
Those centralized databases become a trusted point-of-failure that are worthy targets of attack and exploitation.

Instead, we're going to write a version of RSVP that using a single instance.

If you're only familiar with Reach from the @{seclink("tut")} tutorial, then you should be wondering, "But how are you going to let any number of Guests join? And where will you store their information?"
We're going to use two new concepts to implement this:
1. APIs --- which are a source of publications that do not originate in a participant are a source of asynchronous input events to the consensus network.
1. Mappings --- which are a database of values that grow linearly over the lifetime of the program.

We will define an API that Guests can call to register for an Event and we'll define another API that the Host can call whenever they want to report whether a Guest showed up.

We will define a Mapping that records whether an Guest has registered and we will clear that record when the Host reports their attendance.

Since API publications do not come from participants, but could come from any number of actors in the consensus network, including other contracts, AND because Mappings can grow linearly in the number of actors that interact with the program, there is no limit to the number of Guests!

The overall structure of our application will be:
1. The Host creates an application instance.
1. Guests repeatedly make reservations, before the deadline.
1. The Host repeatedly reports whether Guests come.
1. The program ends when all reserved Guests are accounted for.

Let's dig into the code!

## {#tut-rsvp5-rsh} Programming with APIs and Maps

XXX

XXX show centralized version

XXX talk about pros & cons

XXX show test framework

XXX talk about pros & cons

XXX talk about views & events

XXX show views & events

XXX show interactive version

XXX talk about Web version
