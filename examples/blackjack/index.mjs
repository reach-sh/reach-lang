import { loadStdlib, ask } from "@reach-sh/stdlib"
import * as backend from "./build/index.main.mjs"
const stdlib = loadStdlib(process.env)

function valueToName(value) {
  switch (value) {
    case 1: {
      return "Ace"
    }
    case 2: {
      return "Two"
    }
    case 3: {
      return "Three"
    }
    case 4: {
      return "Four"
    }
    case 5: {
      return "Five"
    }
    case 6: {
      return "Six"
    }
    case 7: {
      return "Seven"
    }
    case 8: {
      return "Eight"
    }
    case 9: {
      return "Nine"
    }
    case 10: {
      return "Ten"
    }
    case 11: {
      return "Jack"
    }
    case 12: {
      return "Queen"
    }
    case 13: {
      return "King"
    }
  }
}

function createDeck() {
  const deck = []
  for (let i = 0; i < 4; i++) {
    for (let j = 1; j < 14; j++) {
      deck.push({ suit: i, value: j >= 11 ? 10 : j, name: valueToName(j) })
    }
  }
  return deck
}

function shuffleDeck(unshuffledDeck) {
  const shuffledDeck = [...unshuffledDeck]
  for (let i = shuffledDeck.length - 1; i > 0; i--) {
    let j = Math.floor(Math.random() * i)
    let temp = shuffledDeck[i]
    shuffledDeck[i] = shuffledDeck[j]
    shuffledDeck[j] = temp
  }
  return shuffledDeck
}

function getStartingHands(deck) {
  const aliceHand = []
  const bobHand = []

  aliceHand.push(deck.pop())

  bobHand.push(deck.pop())

  aliceHand.push(deck.pop())

  bobHand.push(deck.pop())

  return { aliceHand, bobHand }
}

function suitValueToName(value) {
  switch (value) {
    case 0: {
      return "Hearts"
    }
    case 1: {
      return "Diamonds"
    }
    case 2: {
      return "Clubs"
    }
    case 3: {
      return "Spades"
    }
  }
}

function handToString(hand) {
  let string = ""
  for (let i = 0; i < hand.length; i++) {
    const value = hand[i]
    if (i == 0) {
      string += `${valueToName(value.value)} of ${suitValueToName(value.suit)} `
    } else {
      string += `and ${valueToName(value.value)} of ${suitValueToName(
        value.suit
      )}`
    }
  }
  return string
}

const OUTCOME = ["Alice wins", "Tie", "Bob wins"]

export function trimByteString(str = "") {
  return str.replace(/\0/g, "")
}

async function main() {
  const startingBalance = stdlib.parseCurrency(100)

  const accAlice = await stdlib.newTestAccount(startingBalance)
  const accBob = await stdlib.newTestAccount(startingBalance)

  const ctcAlice = accAlice.contract(backend)

  const ctcBob = accBob.contract(backend, ctcAlice.getInfo())

  const newDeck = createDeck()
  const shuffledDeck = shuffleDeck(newDeck)

  const { aliceHand, bobHand } = getStartingHands(shuffledDeck)

  const fmt = (x) => stdlib.formatCurrency(x, 4)
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who))
  const beforeAlice = await getBalance(accAlice)
  const beforeBob = await getBalance(accBob)

  const Player = (Who) => ({
    getStartingHand: () => {
      if (Who == "Alice") {
        console.log(`Alice's starting hand is ${handToString(aliceHand)}.`)
        return aliceHand
      } else {
        console.log(`Bob's starting hand is ${handToString(bobHand)}.`)
        return bobHand
      }
    },
    hit: (currentHandVal) => {
      console.log(`${Who}'s current hand value is ${currentHandVal}.`)
      let newCard = shuffledDeck.pop()
      console.log(
        `${Who} hits and draws a ${newCard.name} of ${suitValueToName(
          newCard.suit
        )}.`
      )
      if (newCard.name === "Ace") {
        if (currentHandVal + 11 > 21) {
          newCard.value = 1
        } else {
          newCard.value = 11
        }
      }
      const newHandValue = Number(currentHandVal) + Number(newCard.value)
      if (newHandValue > 21) {
        console.log(`${Who} busts.`)
      }
      if (newHandValue === 21) {
        console.log(`${Who} hits 21!`)
      }
      return newCard
    },
    keepGoing: (currentHandVal) => {
      return currentHandVal > 16 ? false : true
    },
    checkForInitialBlackjack: (hand) => {
      const handValue = hand
        .map((x) =>
          trimByteString(x.name) === "Ace"
            ? 11
            : stdlib.bigNumberToNumber(x.value)
        )
        .reduce((prev, curr) => prev + curr, 0)
      if (handValue === 21) {
        console.log(`${Who} has a blackjack!`)
        return true
      }
      return false
    },
    seeOutcome: (winner) => {
      console.log(`${Who} saw outcome ${OUTCOME[winner]}`)
    },
    informTimeout: () => {
      console.log(`${Who} observed a timeout`)
    },
  })

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...Player("Alice"),
      wager: stdlib.parseCurrency(5),
    }),
    backend.Bob(ctcBob, {
      ...Player("Bob"),
      acceptWager: (amt) => {
        console.log(`Bob accepts the wager of ${fmt(amt)}.`)
      },
    }),
  ])

  const afterAlice = await getBalance(accAlice)
  const afterBob = await getBalance(accBob)

  console.log(`Alice's balance went from ${beforeAlice} to ${afterAlice}.`)
  console.log(`Bob's balance went from ${beforeBob} to ${afterBob}.`)
}

main()
