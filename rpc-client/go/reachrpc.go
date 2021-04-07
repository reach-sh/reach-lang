package reachrpc

import (
  "fmt"
  "log"
  "net"
  "os"
  "reflect"
  "strings"
  "time"
  "crypto/tls"
  "encoding/json"
  "io/ioutil"
  "net/http"
)

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

type sigRpc          = func(string, ...interface{}) interface{}
type sigRpcCallbacks = func(string, string, map[string]interface{})

func Mk(mopts ...map[string]string) (sigRpc, sigRpcCallbacks) {
  opts := map[string]string {}
  if len(mopts) > 0 {
    opts = mopts[0]
  }

  host := os.Getenv("REACH_RPC_SERVER")
  if val, ok := opts["host"]; ok {
    host = val
  }
  port := os.Getenv("REACH_RPC_PORT")
  if val, ok := opts["port"]; ok {
    port = val
  }
  key  := os.Getenv("REACH_RPC_KEY")
  if val, ok := opts["key"]; ok {
    key = val
  }
  timeouts := "5"
  if os.Getenv("REACH_RPC_TIMEOUT") != "" {
    timeouts = os.Getenv("REACH_RPC_TIMEOUT")
  }
  if val, ok := opts["timeout"]; ok {
    timeouts = val
  }
  skipVerify := false
  if os.Getenv("REACH_RPC_TLS_REJECT_UNVERIFIED") != "" {
    skipVerify = os.Getenv("REACH_RPC_TLS_REJECT_UNVERIFIED") == "0"
  }
  if val, ok := opts["verify"]; ok {
    skipVerify = val == "0"
  }

  if skipVerify {
    fmt.Printf("\n*** Warning! TLS verification disabled! ***\n\n")
    fmt.Printf(" This is highly insecure in Real Life™ applications and must\n")
    fmt.Printf(" only be permitted under controlled conditions (such as\n")
    fmt.Printf(" during development).\n\n")
  }

  // Wait for RPC server to become available
  timeout := please(time.ParseDuration(fmt.Sprintf("%ss", timeouts))).(time.Duration)
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

  debug := func(s string) {
    if os.Getenv("REACH_DEBUG") != "" {
      fmt.Printf(s)
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
    debug(lab)

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

    debug(fmt.Sprintf("%s ==> %s\n", lab, string(body)))

    return ans
  }

  rpcCallbacks := func(m string, arg string, cbacks map[string]interface{}) {
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
