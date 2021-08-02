# Governance Example Spec

This will provide some simple governance functions as example behavior to be used across projects and as a simple demonstration of best-practices

After the initial scaffold, we may adjust the governance contract to perform initiation and update features for contract configuration, perhaps of some custom data, or perhaps using the existing `stablecoin` example from the plutus monorepo.

We may also move some of the implementations into an open-source library, where possible

This Contract will deal with a custom, pre minted token called GOV.  

in reality this GOV token can be any token at all, the primary purpose of the contract is to allow users to store and retreive GOV tokens, report on GOV token holdings (as they may be used in a vote), and provide rewards.

For this contract, the primitive Plutus API, and not the state machine API will be used.

## Governance Contract:

### Deposit
prerequisites: 
user has specified amount of GOV tokens

input: { amount :: Integer }

behaviors:

user must provide `amount` of GOV tokens to contract or else the contract errors out.
`amount` of GOV tokens associated with this user deposit must be reportable and we must be able to query this information knowing only the user's address PubKey

we should mint xGOV tokens (this may be a configurable behavior in a library function to match the input GOV tokens,  xGOV tokens must be returned in order to claim GOV tokens from `Withdraw`)

user cannot provide negative inputs

### Withdraw

prerequisites: 
user has successfully called deposit, 
user has specified amount of xGOV tokens in their wallet

input: { amount :: Integer }

behavior:
transfer `amount` of user-provided xGOV tokens to contract and burn them
transfer `amount` of GOV tokens to the user

if user does not have provided amount of xGOV,  error.

(because of how xGOV tokens and voting work, you must withdraw and redeposit your GOV tokens for your vote weight to change)

user cannot provide negative inputs

### CreatePoolFactory 
Prerequisites: 
none

input: { factoryParams :: FactoryParams }

behaviour: 
initiate the contract, create the PoolFactory seeded with this CurrencySymbol. 
factoryParams are the minimum open slot range, maximum open slot range, how many slots are required to not overlap (i.e. be after the last Pool), whitelist of 'admins' allowed to create pools (empty means anyone can create them) -- this also gives a unique (up to hash collisions) address to the PoolFactory.

comments:
FactoryParams is the only reason as to why we wouldn't just have Governance be merged with PoolFactory, unless we want to have govenance be paramtrised by them

## PoolFactory Contract
is this the right name for it?

### CreatePool
prerequisites:
none

input: { poolType :: VotingPool VotingDSL + RewardPool Value, openRange :: SlotRange, poolParams :: PoolParams }

behaviour: assumption is that we only want to have fancy systems for voting, not distribution (which is direct and proportional). This simplifies the DSL.
the openRange must be at valid for at least N slots after the last created pool, to allow everyone to participate in all votes/rewards.
this instantiates the appropriate Pool contract

each of the AssetClass'es in Value is distributed independently of all others (otherwise we need to query exhanges to see how much of one is worth the other etc, too much work and may not even have an exchange rate)

## Pool Contract

### Participate
prerequisites:
user provides the amount of xGOV (AssetClass, check that it's xGOV) that was specified

input: { tokens :: AssetClass }

### Close
prerequisites: 
`from now` intersect `slotRange` is empty

input: none

output: distributes the xGOV to their original owners, alongside proportional rewards, if it was a RewardPool
