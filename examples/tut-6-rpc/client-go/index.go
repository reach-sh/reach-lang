package main

import (
  "fmt"
  "log"
  "os"
  "reflect"
  "strings"
  "sync"
  "time"
  "encoding/json"
  "io/ioutil"
  "math/rand"
  "net/http"
)


type account   = float64
type contract  = float64
type bigNumber = map[string]interface {}


func dieIf(err error) {
  if err != nil {
    log.Fatal(err)
    os.Exit(1)
  }
}


func please(a interface{}, err error) interface{} {
  dieIf(err)
  return a
}


func mkRpc() (func(string, ...interface{}) interface{},
              func(string, contract, map[string]interface{})) {

  proto := "http"
  host  := os.Getenv("REACH_RPC_SERVER")
  port  := os.Getenv("REACH_RPC_PORT")

  // TODO wait for server to become available

  rpc := func(m string, args ...interface{}) (interface{}) {
    var ans   interface{}
    var jargs string

    if args == nil {
      jargs = "[]"
    } else {
      jargs = string(please(json.Marshal(args)).([]byte))
    }

    lab  := fmt.Sprintf("RPC %s %s\n", m, jargs)
    fmt.Printf(lab)

    uri  := fmt.Sprintf("%s://%s:%s%s", proto, host, port, m)
    res  := please(http.Post(uri, "application/json", strings.NewReader(jargs))).(*http.Response)
    body := please(ioutil.ReadAll(res.Body)).([]byte)

    res.Body.Close();
    dieIf(json.Unmarshal(body, &ans))

    fmt.Printf("%s ==> %s\n", lab, string(body))

    return ans
  }

  rpcCallbacks := func(m string, arg contract, cbacks map[string]interface{}) {
    var ans interface{}

    vals  := make(map[string]interface{})
    meths := make(map[string]bool)

    for key, val := range cbacks {
      if reflect.ValueOf(val).Kind() == reflect.Func {
        meths[key] = true
      } else {
        vals[key]  = val
      }
    }

    p := rpc(m, arg, vals, meths).(map[string]interface{})

    for true {
      if p["t"] == "Done" {
        break

      } else if p["t"] == "Kont" {

        // Apply interact method to supplied inputs
        if _, exists := meths[p["m"].(string)]; exists {
          cb    := reflect.ValueOf(cbacks[p["m"].(string)])
          args  := p["args"].([]interface{})
          vargs := make([]reflect.Value, len(args))

          for i, a := range args {
            vargs[i] = reflect.ValueOf(a)
          }

          ans = nil
          for _, a := range cb.Call(vargs) {
            ans = a.Interface()
            break
          }

          p = rpc("/kont", p["kid"], ans).(map[string]interface{})

        // Respond with simple field value
        } else {
          ans = vals[p["m"].(string)]
          p   = rpc("/kont", p["kid"], ans).(map[string]interface{})
        }

      } else {
        log.Fatalf("Illegal callback return: %s", p)
        os.Exit(1)
      }
    }
  }

  return rpc, rpcCallbacks
}


func main() {
  fmt.Println("I am the client")
  rpc, rpcCallbacks := mkRpc()

  fmtc := func(i bigNumber) string {
    return rpc("/stdlib/formatCurrency", i, 4).(string)
  }

  getBalance := func(w account) string {
    return fmtc(rpc("/stdlib/balanceOf", w).(bigNumber))
  }

  startingBalance := rpc("/stdlib/parseCurrency", 10).(bigNumber)
  accAlice        := rpc("/stdlib/newTestAccount", startingBalance).(account)
  accBob          := rpc("/stdlib/newTestAccount", startingBalance).(account)

  beforeAlice     := getBalance(accAlice)
  beforeBob       := getBalance(accBob)

  ctcAlice        := rpc("/acc/deploy",  accAlice).(contract)
  aliceInfo       := rpc("/ctc/getInfo", ctcAlice).(interface{})
  ctcBob          := rpc("/acc/attach",  accBob, aliceInfo).(contract)

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

    seeOutcome := func(n bigNumber) {
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
    d["wager"] = rpc("/stdlib/parseCurrency", 5).(bigNumber)

    rpcCallbacks("/backend/Alice", ctcAlice, d)
  }

  playBob := func() {
    defer wg.Done()

    d := player("Bob")
    d["acceptWager"] = func(amt bigNumber) {
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

  rpc("/stop")
}
