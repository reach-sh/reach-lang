---
author: Jay McCarthy
hasOtp: true
menuItem: mi-docs
publishedDate: 2020-08-29T14:00:00
---

# How to protect against non-participation

Non-participation refers to the act of one party ceasing to continue playing their role in an application. In traditional client-server programs, like a Web server, this would be the case of a client stopping sending requests to the server, or the server stopping sending responses to the client. In these sorts of traditional programs, non-participation is an exceptional circumstance that normally leads to an error message for clients and, at most, a log entry for servers. Sometimes traditional programs will need to recycle resources, like network ports, on non-participation, but they would have also needed to do that if the transaction ended by normal means. In other words, for traditional client-server programs, it is not necessary for designers to meticulously consider the consequences of non-participation. In contrast, decentralized applications must be carefully designed with an eye towards their behavior in the face of non-participation. There are two general strategies for dealing with non-participation: punishment and disincentivizing.

## Punishment

The punishment strategy entails allowing the remaining participants of the application to take all of the assets that would have gone to the non-participator and splitting them. This is the strategy used in the tutorial, where Alice’s wager is lost if she fails to send her next publication.

Punishment is a dangerous strategy, because it is difficult to know a priori what the threshold of non-participation is. If you estimate too low, then you will inappropriately punish parties that are honestly delayed. In consensus networks where transactions bid for space in blocks, like Ethereum, low-value transactions may be starved when high-value activity is happening on the network concurrently. Reach allows timeout deadlines to be dynamically set, because the deadline expression of a timeout clause can be any equation over consensus state. This does, however, imply that the application must decide the deadline for a message before or during the immediately preceding message.

## Disincentivizing

The disincentivizing strategy allows all participants to drop participation, but ensures that if they do so, they will punish themselves by forgoing some resource held in escrow by the application. This strategy is only effective if the escrow amount is significantly larger than the payout in a normal execution of the computation. For example, in the Rock, Paper, Scissors! tutorial, we could have had each participant contribute something on the order of ten times that wager into the application, which would not be reimbursed unless the game ended.

Disincentivizing has the advantage of allowing all programs to eventually finish, but the downside of allowing one participant to disrupt all participants by refusing to continue, thereby locking away the escrows of all parties. It is, therefore, better to only use such incentives when combined with judicious timeouts and asymmetry in the program.