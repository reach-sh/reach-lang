# Crowdfunding Smart Contract #
A decentralized crowdfunding smart contract built with Reach

### How it works ##

This crowdfunding smart contract consists of two actors: `Artist` (1) and `Fan` (multiple). The `Artist` user deploys the contract to raise funds for their project and defines a goal. Multiple `Fan` users can connect to the contract and add funds to the crowdfunding campaign by choosing different perks from a list. Fans store their contributions in the contract. At the end of the campaign, the Artist gets the funds transferred to them.


### How to run
Do `$ reach run` or `$ ./reach run` on the home directory, depending on where you reach installed on your environment.

Follow the command line prompts for `Artist` or `Fan` user.

### Future development ###
1. Allow fans to select between all-or-nothing mode (Kickstarter style) or all-or-something (Indiegogo style) campaign modes. In the All-or-nothing crowdfunding mode, the Artist gets the funds in the contract only if thy reach the original goal. The Artist gets all the crowdsourced funds in an All-or-something mode, regardless of the goal reached.

2. Allow users to recollect/harvest the funds from an All-or-nothing campaign if the artist doesn't reach the goal.

3. Allow Artist users to dynamically set the crowdfunding perks outside the contract by referencing a data repository.