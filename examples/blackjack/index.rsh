"reach 0.1"

const [isOutcome, ALICE_WINS, DRAW, BOB_WINS] = makeEnum(3)

const CardSuit = UInt
const CardValue = UInt

const Card = Object({
  suit: CardSuit,
  value: CardValue,
  name: Bytes(8),
})

const Hand = Array(Card, 2)

const Contestant = {
  getStartingHand: Fun([], Hand),
  informTimeout: Fun([], Null),
  keepGoing: Fun([UInt], Bool),
  hit: Fun([UInt], Card),
  seeOutcome: Fun([UInt], Null),
  checkForInitialBlackjack: Fun([Hand], Bool),
}

const FirstPlayer = {
  ...Contestant,
  wager: UInt,
}

const SecondPlayer = {
  ...Contestant,
  acceptWager: Fun([UInt], Null),
}

const getNextHandValue = (card, handVal) => {
  return handVal + card.value
}

const getHandValue = (hand) => {
  const [firstCard, secondCard] = hand

  return firstCard.value + secondCard.value
}

const getWinner = (handA, handB) => {
  if (handA > handB && handA <= 21) {
    return ALICE_WINS
  } else if (handB > handA && handB <= 21) {
    return BOB_WINS
  } else if (handB == handA && handB <= 21 && handA <= 21) {
    return DRAW
  } else if (handA <= 21 && handB > 21) {
    return ALICE_WINS
  } else if (handB <= 21 && handA > 21) {
    return BOB_WINS
  } else {
    return DRAW
  }
}

assert(getWinner(19, 17) == ALICE_WINS)
assert(getWinner(17, 19) == BOB_WINS)
assert(getWinner(18, 18) == DRAW)
assert(getWinner(15, 22) == ALICE_WINS)
assert(getWinner(22, 17) == BOB_WINS)
assert(getWinner(23, 23) == DRAW)

forall(UInt, (handValA) =>
  forall(UInt, (handValB) => assert(isOutcome(getWinner(handValA, handValB))))
)

const DEADLINE = 30

export const main = Reach.App(() => {
  const Alice = Participant("Alice", { ...FirstPlayer })

  const Bob = Participant("Bob", { ...SecondPlayer })

  init()

  const informTimeout = () => {
    each([Alice, Bob], () => {
      interact.informTimeout()
    })
  }
  Alice.only(() => {
    const wager = declassify(interact.wager)
  })
  Alice.publish(wager).pay(wager)
  commit()

  Bob.only(() => {
    interact.acceptWager(wager)
  })
  Bob.pay(wager).timeout(relativeTime(DEADLINE), () =>
    closeTo(Alice, informTimeout)
  )
  commit()

  Alice.only(() => {
    const initialHandA = declassify(interact.getStartingHand())
    const hasBlackjackA = declassify(
      interact.checkForInitialBlackjack(initialHandA)
    )
  })
  Alice.publish(initialHandA, hasBlackjackA)
  commit()

  Bob.only(() => {
    const initialHandB = declassify(interact.getStartingHand())
    const hasBlackjackB = declassify(
      interact.checkForInitialBlackjack(initialHandB)
    )
  })
  Bob.publish(initialHandB, hasBlackjackB)

  const [keepGoing, handValA, handValB] = parallelReduce([
    !hasBlackjackA && !hasBlackjackB,
    getHandValue(initialHandA),
    getHandValue(initialHandB),
  ])
    .invariant(balance() == 2 * wager)
    .while(keepGoing)
    .case(
      Alice,
      () => ({
        when: declassify(interact.keepGoing(handValA)),
      }),
      (_) => {
        commit()
        Alice.only(() => {
          const card = declassify(interact.hit(handValA))
        })
        Alice.publish(card)
        return [true, getNextHandValue(card, handValA), handValB]
      }
    )
    .case(
      Bob,
      () => ({
        when: declassify(interact.keepGoing(handValB)),
      }),
      (_) => {
        commit()
        Bob.only(() => {
          const card = declassify(interact.hit(handValB))
        })
        Bob.publish(card)
        return [true, handValA, getNextHandValue(card, handValB)]
      }
    )
    .timeout(relativeTime(DEADLINE), () => {
      Anybody.publish()
      return [false, handValA, handValB]
    })

  const winner = getWinner(handValA, handValB)

  each([Alice, Bob], () => {
    interact.seeOutcome(winner)
  })

  const [forAlice, forBob] =
    winner == 0 ? [2, 0] : winner == 2 ? [0, 2] : [1, 1]

  transfer(forAlice * wager).to(Alice)
  transfer(forBob * wager).to(Bob)
  commit()

  exit()
})
