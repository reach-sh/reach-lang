package main

import (
  "fmt"
  "sync"
  "time"
  "math/rand"
)

// This example imports a copied version of `reachrpc` directly from the
// filesystem in order to remain in-sync with the repository's client code, but
// frontend authors will normally import from GitHub like so:
// import reachrpc "github.com/reach-sh/reach-lang/rpc-client/go"
import "reachrpc"

type jsono = map[string]interface {}


func main() {
  rpc, rpcCallbacks := reachrpc.Mk()

  fmtc := func(i jsono) string {
    return rpc("/stdlib/formatCurrency", i, 4).(string)
  }

  getBalance := func(w string) string {
    return fmtc(rpc("/stdlib/balanceOf", w).(jsono))
  }

  startingBalance := rpc("/stdlib/parseCurrency", 10).(jsono)
  accAlice        := rpc("/stdlib/newTestAccount", startingBalance).(string)
  accBob          := rpc("/stdlib/newTestAccount", startingBalance).(string)

  beforeAlice     := getBalance(accAlice)
  beforeBob       := getBalance(accBob)

  ctcAlice        := rpc("/acc/deploy",  accAlice).(string)
  aliceInfo       := rpc("/ctc/getInfo", ctcAlice).(interface{})
  ctcBob          := rpc("/acc/attach",  accBob, aliceInfo).(string)

  HAND            := [3]string{"Rock", "Paper", "Scissors"}
  OUTCOME         := [3]string{"Bob wins", "Draw", "Alice wins"}

  player := func(who string) map[string]interface{} {
    getHand := func() int {
      // https://golang.org/pkg/math/rand/#example_Intn
      rand.Seed(time.Now().UnixNano())
      hand := rand.Intn(3)
      fmt.Printf("%s played %s\n", who, HAND[hand])
      return hand
    }

    informTimeout := func() {
      fmt.Printf("%s observed a timeout\n", who)
    }

    seeOutcome := func(n jsono) {
      o := int(rpc("/stdlib/bigNumberToNumber", n).(float64))
      fmt.Printf("%s saw outcome %s\n", who, OUTCOME[o])
    }

    return map[string]interface{} {
      "stdlib.hasRandom": true,
      "getHand":          getHand,
      "informTimeout":    informTimeout,
      "seeOutcome":       seeOutcome,
    }
  }

  wg := new(sync.WaitGroup)
  wg.Add(2)

  playAlice := func() {
    defer wg.Done()

    d := player("Alice")
    d["wager"] = rpc("/stdlib/parseCurrency", 5).(jsono)

    rpcCallbacks("/backend/Alice", ctcAlice, d)
  }

  playBob := func() {
    defer wg.Done()

    d := player("Bob")
    d["acceptWager"] = func(amt jsono) {
      fmt.Printf("Bob accepts the wager of %s\n", fmtc(amt))
    }

    rpcCallbacks("/backend/Bob", ctcBob, d)
  }

  go playAlice()
  go playBob()
  wg.Wait()

  afterAlice := getBalance(accAlice)
  afterBob   := getBalance(accBob)

  fmt.Printf("Alice went from %s to %s\n", beforeAlice, afterAlice)
  fmt.Printf("  Bob went from %s to %s\n", beforeBob,   afterBob)

  rpc("/forget/acc", accAlice, accBob)
  rpc("/forget/ctc", ctcAlice, ctcBob)
}
