# {#guide-staking} Example: Staking and Unstaking Tokens

Reach makes DApp development safer, faster, and more secure.
This guide offers a real example of the advantages of using Reach.

Here's an example provided by Austin Wilshire at [xBacked](https://twitter.com/xbacked).
Follow Austin on Twitter [@awoldes](https://twitter.com/awoldes).

---

Imagine you want to stake a token and at a later date unstake it with all the rewards.
Let's look at a snippet from the perspective of a Deployer.

As the deployer, you want to set the reward rate, `{!rsh} Token` being staked, the rewarded `{!rsh} Token`, and the initial supply of the reward token.

``` rsh
// Initiate the Deployer's local state
Deployer.only(() => {
  // this = address doing transaction
  const deployerAddr = this;
  // Declassify data and store in constants
  const stakeToken = declassify(interact.setStakeToken());
  const rewardRate = declassify(interact.setRewardRate());
  const rewardToken = declassify(interact.setRewardToken());
  const initialSupply = declassify(interact.setInitialSupply());
});

// Publish this information to global state
Deployer.publish(
  deployerAddr,
  stakeToken,
  rewardRate,
  rewardToken,
  initialSupply,
);
```

After this, the contract is fully configured and deployed.
The next step is to enable users to interact with it.

# Staking

First, we want to enable a user to add a stake.
We do this by adding an API call so that when the user calls the function, they will pay `{!rsh} tokenAmt` of the `{!rsh} stakeToken` which the Deployer set.

``` rsh
.api(User.stakeTokens,
  (tokenAmt) => {
    // Valid that the user must stake more than 1 token.
    // They will receive the message on the frontend if they don't.
    assume(tokenAmt >= 1, 'Must stake at least 1 token');
  },
  (tokenAmt) => {
    // This means the user will:
    // - pay 0 network tokens; and,
    // - pay tokenAmt in stakeTokens
    return [0, [tokenAmt, stakeToken]];
  },
  (tokenAmt, apiReturn) => {
    // This require matches the assume above
    require(tokenAmt >= 1, 'Must stake at least 1 token');

    // We record in a Map that the user staked
    userStakes[this] = {
        stake: tokenAmt,
        pendingRewards: 0,
    };
    // Return to the API caller
    apiReturn(true);
    // Update global state
    return rewardSupply;
  }
)
```

In a full implementation, we would enable users to stake multiple times and
combine their stakes.
(This version resets the stake each time, so it is dangerous and would lose
track of a user's funds.)

# Unstaking

Similarly, we want the user to be able to unstake and claim their rewards.
We do this with an API call as well.

``` rsh
.api(User.unstakeAndClaim,
  () => {
    // Ensure that rewards are available
    assume(balance(rewardToken) > 0, 'No rewards left');
  },
  () => {
    // Transfer nothing to the contract
    return [0, [0, stakeToken]];
  },
  (apiReturn) => {
    require(balance(rewardToken) > 0, 'No rewards left');
    const userState = userStakes[this];
    // Pay acrued rewards
    transfer([[userState.pendingrewards, rewardToken]]).to(this);

    // Reset the staked amount
    userStakes[this] = {
        stake: 0,
        pendingRewards: 0,
    }
    // Return to the API caller
    apiReturn(true)
    // Update global state
    return (rewardSupply - userState.pendingRewards);
  }
)
```

In a full implementation, we would have to specify how the `pendingRewards`
value was computed.
We could make it automatic by connecting it to something like `{!rsh}
lastConsensusTime`.

# Elided Reach

We haven't shown the entire Reach program.
We didn't show the definition of the participants, APIs, or Views.
We didn't show the main `{!rsh} parallelReduce` or its invariants.

# Connecting from JavaScript

Next, we'll want to use this program in a Web application.
In this code, we assume that the Web application is written with React.

We use the Reach standard library to connect to the [contract](##ref-frontends-js-ctc), define the User's [API](##ref-programs-appinit-api) calls, and the [View](##ref-programs-appinit-view).

``` js
// Connect the account
const account = await stdlib.getDefaultAccount();
// Construct a contract handle
const ctcInfo = account.contract(backend, CONTRACT_INFO);
// Defines all User api calls (stakeTokens, unstaketokens)
const userApi = contract.a.User;
// Defines the View
const userView = contract.v.read;
```

Next, we get the user's state by reading the view:

``` js
useEffect(async() => {
    // Get user's state from application
    const userState = await userView.read(account.addr);
    setUserState(userState);
}, []);
```

Then, create an event listener that connects to the API calls:

``` js
<div>
  {/* On button clicks, these functions will trigger the call */}
  <button onClick={async () => await userApi.stakeTokens(amt)}>
    Stake Tokens
  </button>

  <button onClick ={async () => await userApi.unstakeTokens(amt)}>
    Unstake Tokens + {userState.pendingRewards} rewards
  </button>
</div>
```

# Conclusion

This demonstrates the essential (but not quite complete) elements of implementing a DApp with Reach.
There's no magic involved, but it definitely feels like there is when you're building a Reach application.
As a developer, you don't need to think about forming transaction groups, SDK calls, "opting in", or "ASA ids".
Because you don't think about these things, your application works on multiple
networks.
You focus on the business logic of your DApp and plug into the consensus network of your choice.
