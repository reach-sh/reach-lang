package main

import (
  "fmt"
  "log"
  "net"
  "os"
  "reflect"
  "strings"
  "sync"
  "time"
  "crypto/tls"
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

  host  := os.Getenv("REACH_RPC_SERVER")
  port  := os.Getenv("REACH_RPC_PORT")
  key   := os.Getenv("REACH_RPC_KEY")

  skipVerify := os.Getenv("REACH_RPC_TLS_REJECT_UNVERIFIED") == "0"
  if skipVerify {
    fmt.Printf("\n*** Warning! TLS verification disabled! ***\n\n")
    fmt.Printf(" This is highly insecure in Real Life™ applications and must\n")
    fmt.Printf(" only be permitted under controlled conditions (such as\n")
    fmt.Printf(" during development).\n\n")
  }

  // Wait for RPC server to become available
  timeout := please(time.ParseDuration("5.0s")).(time.Duration)
  began   := time.Now()
  for true {
    conn, err := net.DialTimeout("tcp", fmt.Sprintf("%s:%s", host, port), timeout)
    if err == nil {
      conn.Close()
      break
    } else {
      time.Sleep(please(time.ParseDuration("0.01s")).(time.Duration))

      if time.Since(began) > timeout {
        log.Fatalf("Waited too long for the port %s on host %s " +
                   "to start accepting connections", port, host)
        os.Exit(1)
      }
    }
  }

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

    uri  := fmt.Sprintf("https://%s:%s%s", host, port, m)
    tls  := &tls.Config     { InsecureSkipVerify: skipVerify, }
    trns := &http.Transport { TLSClientConfig:    tls,        }
    clnt := &http.Client    { Transport:          trns,       }
    req  := please(http.NewRequest("POST", uri, strings.NewReader(jargs))).(*http.Request)

    req.Header.Add("Content-Type", "application/json; charset=utf-8")
    req.Header.Add("X-API-Key",    key)

    res  := please(clnt.Do(req)).(*http.Response)
    body := please(ioutil.ReadAll(res.Body)).([]byte)
    res.Body.Close();

    if res.StatusCode >= 400 {
      log.Fatal(fmt.Sprintf("RPC %s %s %s\n", m, res.Status, body))
      os.Exit(1)
    }

    dieIf(json.Unmarshal(body, &ans))

    fmt.Printf("%s ==> %s\n", lab, string(body))

    return ans
  }

  rpcCallbacks := func(m string, arg contract, cbacks map[string]interface{}) {
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
        cb    := reflect.ValueOf(cbacks[p["m"].(string)])
        args  := p["args"].([]interface{})
        vargs := make([]reflect.Value, len(args))

        for i, a := range args {
          vargs[i] = reflect.ValueOf(a)
        }

        var ans []interface{} = []interface{}{p["kid"]}
        res := cb.Call(vargs)

        if len(res) == 0 {
          ans = append(ans, nil)
        } else {
          for _, a := range res {
            ans = append(ans, a.Interface())
          }
        }

        p = rpc("/kont", ans...).(map[string]interface{})

      } else {
        log.Fatalf("Illegal callback return: %s\n", p)
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

  // rpc("/stop")
}
