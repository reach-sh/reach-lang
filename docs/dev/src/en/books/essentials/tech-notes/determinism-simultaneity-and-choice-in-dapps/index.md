---
author: Jay McCarthy
hasOtp: false
menuItem: mi-docs
publishedDate: 2020-08-29T14:00:00
---

# Determinism, simultaneity, and choice in decentralized applications

The structure of a Reach computation is deterministic, because at each point in a computation, all participants agree on which publication is the next one in the computation. If this were not the case, then different participants may attempt to pursue different paths through a computation and thereby reach different values at the end. This deterministic structure, however, does not mean that the participant that provides the publication must be fixed, merely that which publication event is next must be fixed; see for example race expressions and the the guide section on races for an elaboration of this point. Even in the presence of this non-determinism in actors, Reach programs remain deterministic in their structure.

However, many developers think of their application as having a step when two participants act simultaneously. For example, in a game of Rock, Paper, Scissors! in the real world, both players simultaneously choose their hands. Similarly, a rental agreement gives both the landlord and the tenant the ability to cancel the agreement (subject to some penalty) at any time. In both of these cases, it is not clear how to understand this interaction as being sequential and deterministic.

At first glance, these situations appear different. In the first, the participants are doing "the same thing" simultaneously, because both will submit a hand eventually; while in the second, they are doing "something different", because only one of them will actually end the agreement early. However, both of these situations are actually identical, because in the second case they are both simultaneously deciding whether they will end early. In the first case, the participants are submitting one of three values (Rock, Paper, or Scissors), while in the second they are submitting one of two (Leave or Stay).

In such situations, in a decentralized application, the program must agree that one participant acts first. The important thing to realize is that "simultaneity" is not the same thing as "non-determinism". The pertinent design detail is whether one participant has an advantage for going in any particular order. If there is no advantage for either place, then the developer can arbitrarily decide to go in one order. If there is an advantage, then a commitment strategy similar to the Rock, Paper, Scissors! tutorial should be used.

For example, in the rental agreement, if we felt there was no advantage for going second, then we could have either the landlord or the tenant go first. But, is there no advantage? If both the tenant and the landlord want to exit in the same month, but the landlord goes first, then the landlord would suffer a loss of their deposit; but, if they fairly shared their choice at the same time, then they could both be refunded in this case. This is an example of the Pareto improvements that are possible in decentralized applications relative to existing institutions.