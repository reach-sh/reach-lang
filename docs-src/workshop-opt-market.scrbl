#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-opt-market")
@title[#:version reach-vers #:tag TAG]{Workshop: Optimization Market}

There are certain cases where you want to solve a particular NP-hard problem. These problems can be Optimization problems, where you have to maximize/minimize the value of a function. These kinds of problems arise a lot in the domain of scientific computing. But, it might be the case that you do not have the resources to carry out the computation yourself. You might be interested in paying someone else to do it for you. Or even better, let the world know that you want to solve this problem, and the one who solves it best and solves it first, gets some prize money. It makes perfect sense to implement it as a smart contract so that no one cheats.

In this workshop, we will be understanding and solving this problem of implementing Optimization Market in Reach Lang. 

@(workshop-init TAG)

@(drstep-pr TAG)

The overall purpose of our application would be:
@itemlist[
 @item{Deployer can decide the function to optimize and an amount of funds to give as wager.}
 @item{Contestants try to optimize the function. If they find a probable input, submit it on the application.}
 @item{The wager gets transferred to the contestant who submits the best input value.}
]

With this in mind, let's answer the questions:
@itemlist[
 @item{What are the participants of the program?}
 @item{What information do they know at the start of the program?}
 @item{What information are they going to discover and use in the program?}
 @item{What funds change ownership during the application and how?}
]

@(drstep-pr-stop)

Let's see how your answers compare to our answers:

@itemlist[
 @item{The program has three participants: Deployer, who initiates the application, Contestant, who participates in the competition and the Spectator, who only reads the current Leaderboard and writes nothing.}
 @item{Deployer starts knowing the amount it wants to transfer, deadline of the contest and the function it wants to optimize.}
 @item{By personal computation, the Contestants get to know their inputs, which are candidate optimized input to the target function. Whenever they believe they have a probable answer, they submit the values.}
 @item{The funds start with Deployer and then move to the Contestant that submits the best answer.}
]

A unique thing about this program is that the program doesn't end after a fixed number of messages, but after a deadline. A contestant is accepted and can give inputs as long as the deadline isn't passed. This needs us to write the code using a special construct called parallel reduce, which we will discuss later in the coming sections.

@(drstep-dd TAG)

Now comes the step where we define the @tech{participant interact interface} for each participant. Some of the questions that can help in deciding these data definitions are -

@itemlist[
 @item{How do Deployer and Contestant differ? What are the duties of each of them?}
 @item{What are the values assosiated with all three types of participants?}
]

@(drstep-dd-stop)

Let's compare notes again.
Here's what we wrote in our program:

@reachex[#:show-lines? #t "workshop-opt-market/index.rsh"
         #:link #t
         'only 6 32 "  // ..."]

In Funder interface, we have a function called getWager to get wager amout and the deadline of the competition in number of transaction blocks. Post wager is a function to inform the frontend that the wager has been set. You might wonder where the bounty function is. It is included from a seperate file and the backend needs to be recompiled for different bounty functions.

In Contestant interface, we have submitValue function, which is used by the contestant to submit a possible input to the bounty function. The rest three functions are for informing the frontends of users about bounty, winners and tell if the submission was successful.

In the Monitor interface (we gave it the name Monitor because it just monitors the submission and do not make any changes to the contract), we only have one function called seeSubmission, which takes a note about every submission made.

@(drstep-cc TAG)

Now, we can write down the structure of communication and action in our application.
Try this on your own based on your experience with @secref["workshop-hash-lock"].

@(drstep-cc-stop1)

Here's what we wrote:
@reach{
 // 1. Funder sets the bounty details (amount and deadline)
 // 2. Inform all contestants about the amount and deadline.
 // 3. For each submission by a contestant, calculate the bounty function and see if it beats the current winner.
 // 4. Inform the monitor about the submission to maintain the submission history.
 // 5. After deadline passes, see who submitted the best input and the contract pays that person.
}

Till now, as in the previous examples, we always had a static number of participants. But this time, as we can see in the 3rd point, we have a class of participants called contestants and they can join any time. This makes the backend a bit more complex. So to implement such a backend, we would make use of @reachin{parallelReduce}. But before that let's just write the remaining logic.

@(drstep-cc-stop2)

The body of the application where the funder sets the bounty should look something like this:

@reach{
  Funder.publish(amt, deadline)
      .pay(amt);
}

Now let's ignore the parallel reduce logic and assume that one contestant submits, then, this should be what the code should look like:

@reach{

  // evaluated the bounty function
  const evaluatedValue = bountyFunction(inputValue);

  const newEntry = {
      accountAddress: currentContestant,
      inputValue: inputValue,
      returnValue: evaluatedValue,
  };

  // Check if the current submission gives a better
  // output than the leader
  const newWinner = evaluatedValue > currentWinner.returnValue ? newEntry : currentWinner;

}

We got the logic right. Now we have to convert this into Parallel Reduce logic. 

@reachex[#:show-lines? #t "workshop-opt-market/index.rsh"
         #:link #t
         'only 58 106 "  // ..."]

Finally, we need to transfer the wager to the winner.

@reach{
  transfer(balance()).to(currentWinner.accountAddress);
  commit();
}

@(drstep-ai TAG)

There are no interesting assertions needed in this program other than the parallelReduce invariant that the total balance of the contract should always be equal to the amount of the wager set by the funder.


@(drstep-ii TAG)

Next, we need to insert the appropriate calls to @reachin{interact}. As our frontend needs to know at which stage the program is, we have made special functions like informSuccess to just inform the frontend that a particular step has completed.

@(drstep-ii-stop)

Let's look at our whole program now:

@reachex[#:show-lines? #t "workshop-opt-market/index.rsh"
         #:link #t]

@(drstep-de TAG)

Deploying this application needs a file 'bountyFunction.rsh' in the same directory with a function 'bountyFunction' exported which accepts an UInt as argument and returns an UInt. This needs to be set by the funder before compiling and deploying.

Now for the frontend, we have a very basic frontend with it which makes use of all the interact functions and makes the bare minimum frontend for this backend, but we have developed a more complex react frontend for it @link["https://github.com/optymtech/optym/tree/db787039c68ac527a4532d38d8da2243a32c0459"]{here}.

@reachex[#:show-lines? #t "workshop-opt-market/index.mjs"
         #:link #t]

Running this code generates output:

@verbatim{
$ ../reach run
Contestant 2 saw a bounty of 20000000000000000000 and deadline 50
Contestant 2 submitted 0
Contestant 0 saw a bounty of 20000000000000000000 and deadline 50
Contestant 0 did not submit
Contestant 1 saw a bounty of 20000000000000000000 and deadline 50
Contestant 1 submitted 3
Contestant 3 saw a bounty of 20000000000000000000 and deadline 50
Contestant 3 submitted 13
Post Wager
Monitor 1 saw 0xED8B9F63d3Ea1ED7DDbBA483D0c2bFb3ef7F24B7, 0, 0
Contestant 3 saw status: true
Contestant 2 saw status: true
Contestant 1 saw status: true
Contestant 3 submitted 24
Contestant 2 submitted 3
Contestant 1 did not submit
Contestant 0 saw status: true
Contestant 0 submitted 20
Monitor 1 saw 0x4EB23b472b194CbFA7286D9EA143a4d570B63434, 24, 24
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 3 saw status: true
Contestant 3 submitted 0
Contestant 0 saw status: true
Contestant 0 submitted 15
Monitor 1 saw 0x4EB23b472b194CbFA7286D9EA143a4d570B63434, 0, 0
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 0 saw status: true
Contestant 3 saw status: true
Contestant 0 submitted 5
Contestant 3 did not submit
Contestant 2 saw status: true
Contestant 2 submitted 23
Monitor 1 saw 0xED8B9F63d3Ea1ED7DDbBA483D0c2bFb3ef7F24B7, 23, 23
Contestant 0 saw status: true
Contestant 0 did not submit
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 3 saw status: true
Contestant 3 submitted 5
Monitor 1 saw 0x4EB23b472b194CbFA7286D9EA143a4d570B63434, 5, 5
Contestant 0 saw status: true
Contestant 0 did not submit
Contestant 3 saw status: true
Contestant 3 did not submit
Contestant 1 saw status: true
Contestant 1 submitted 2
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 2 saw status: true
Contestant 2 submitted 6
Monitor 1 saw 0x3509C52778a832492D336A4aE820F77E773EAd3A, 2, 2
Contestant 0 saw status: true
Contestant 1 saw status: true
Contestant 0 submitted 25
Contestant 1 did not submit
Contestant 3 saw status: true
Contestant 3 submitted 25
Monitor 1 saw 0xED8B9F63d3Ea1ED7DDbBA483D0c2bFb3ef7F24B7, 6, 6
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 2 saw status: true
Contestant 2 submitted 12
Contestant 0 saw status: true
Contestant 0 submitted 6
Contestant 3 saw status: true
Contestant 3 did not submit
Contestant 3 saw status: true
Contestant 3 did not submit
Monitor 1 saw 0xED8B9F63d3Ea1ED7DDbBA483D0c2bFb3ef7F24B7, 12, 12
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 0 saw status: true
Contestant 1 saw status: true
Contestant 0 did not submit
Contestant 1 submitted 5
Contestant 3 saw status: true
Contestant 3 submitted 19
Monitor 1 saw 0x3509C52778a832492D336A4aE820F77E773EAd3A, 5, 5
Contestant 2 saw status: true
Contestant 1 saw status: true
Contestant 2 did not submit
Contestant 1 submitted 17
Contestant 0 saw status: true
Contestant 0 submitted 19
Contestant 2 saw status: true
Contestant 2 did not submit
Monitor 1 saw 0x4EB23b472b194CbFA7286D9EA143a4d570B63434, 19, 19
Contestant 3 saw status: true
Contestant 3 submitted 19
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 0 saw status: true
Contestant 0 submitted 20
Monitor 1 saw 0x4EB23b472b194CbFA7286D9EA143a4d570B63434, 19, 19
Contestant 1 saw status: true
Contestant 1 submitted 2
Contestant 0 saw status: true
Contestant 0 submitted 19
Contestant 3 saw status: true
Contestant 3 submitted 7
Contestant 2 saw status: true
Contestant 2 did not submit
Monitor 1 saw 0x3509C52778a832492D336A4aE820F77E773EAd3A, 2, 2
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 3 saw status: true
Contestant 3 did not submit
Contestant 0 saw status: true
Contestant 0 submitted 16
Contestant 1 saw status: true
Contestant 1 submitted 2
Monitor 1 saw 0x692b676C61756506DAAF521e7f3B4D9E42f5fF18, 16, 16
Contestant 1 saw status: true
Contestant 0 saw status: true
Contestant 1 submitted 0
Contestant 0 did not submit
Contestant 3 saw status: true
Contestant 3 did not submit
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 2 saw status: true
Contestant 2 submitted 28
Contestant 3 saw status: true
Contestant 3 did not submit
Monitor 1 saw 0x3509C52778a832492D336A4aE820F77E773EAd3A, 0, 0
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 0 saw status: true
Contestant 0 did not submit
Monitor 1 saw 0xED8B9F63d3Ea1ED7DDbBA483D0c2bFb3ef7F24B7, 28, 28
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 2 saw status: true
Contestant 2 submitted 7
Contestant 0 saw status: true
Contestant 0 submitted 3
Contestant 3 saw status: true
Contestant 3 submitted 14
Monitor 1 saw 0xED8B9F63d3Ea1ED7DDbBA483D0c2bFb3ef7F24B7, 7, 7
Contestant 3 saw status: true
Contestant 3 did not submit
Contestant 1 saw status: true
Contestant 1 submitted 23
Contestant 2 saw status: true
Contestant 0 saw status: true
Contestant 2 submitted 22
Contestant 0 submitted 28
Monitor 1 saw 0x3509C52778a832492D336A4aE820F77E773EAd3A, 23, 23
Contestant 0 saw status: true
Contestant 0 did not submit
Contestant 2 saw status: true
Contestant 2 did not submit
Contestant 1 saw status: true
Contestant 1 did not submit
Contestant 3 saw status: true
Contestant 3 did not submit
Contestant 0 went to 99.9999.
Contestant 1 went to 99.9999.
Contestant 2 went to 119.9999.
Contestant 3 went to 99.9999.
}

@section[#:tag (format "~a-dns" TAG)]{Discussion and Next Steps}

Great job! You can now host similar competitions on the blockchain. 

If you found this workshop rewarding, please let us know on @(the-community-link)!

We can generalize this to different types of competitions - like competetive coding contests. These can become a little more complex, but with the power of reach, we can make complex applications in just a couple hundred of lines of code.

This application is deployed as a backend of @link["https://optym.tech"]{Optym}, an application made during Universities Unchained Bounty Hack.

