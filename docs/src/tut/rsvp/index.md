# {#tut-rsvp} Répondez S'il Vous Plaît

This tutorial walks through the creation of a simple decentralized application.
It assumes a basic familiarity with Reach, as though you've gone @{seclink("tut")} tutorial, but does not dwell on intimate details of it.
Similarly, it assumes that you [have Reach installed](##quickstart) and are comfortable using it.

## {#tut-rsvp1} Respond, If You Please

In this tutorial, we'll be building a basic DApp for running events.
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
In that sense, we've just spoken about "De-Ce-DApps" and "De-De-DApps" as opposed to "Ce-Ce-DApps".

We are going to demonstrate the "decentralized" style, because it introduces fewer new Reach concepts, but we believe that the "centralized" design is more appropriate for this application.

## {#tut-rsvp4} Test First, but Verify

But, rather than jumping into the Reach program, we're going to put our money where our mouth is and write a test scenario corresponding to Buffy's Birthday Bash.
We'll demonstrate how to use Reach's testing tools to write a convenient testing framework customized for your application.
We'll show the tests, then the framework, then the Reach code, and show how the Reach code connects to the framework.
This structure is overly complex for this simple application, but it is under complex for what it takes to build a "real" DApp.

XXX show test code

XXX show decentralized code

XXX talk about pros & cons

XXX show test framework

XXX talk about pros & cons

XXX show centralized version

XXX talk about pros & cons

XXX show test framework

XXX talk about pros & cons

XXX talk about views & events

XXX show views & events

XXX show interactive version

XXX talk about Web version
