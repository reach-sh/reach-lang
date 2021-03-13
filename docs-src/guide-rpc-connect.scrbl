#lang scribble/manual
@(require "lib.rkt")

@title[
  #:version reach-vers
  #:tag   "guide-rpc-connect"
  ]{How to connect your language to the Reach RPC server}

It's very easy to create a new a @tech{RPC} client library if one isn't already
available for your platform. This section describes the necessary steps.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "rpc-connect-overview"]{Overview}

As you know from the @seclink["guide-rpc-server-api"]{previous section},
@tech{RPC} client @tech{frontends} communicate via two channels, named
@pyin{rpc} and @pyin{rpc_callbacks}, which are obtained by calling
@pyin{mk_rpc}. Construct these three functions and you'll have already
completed the majority of what's involved in building a new library.

Your code must also address the following requirements:

@itemlist[
  @item{Configuring + communicating via @secref{rpc-connect-tls}}
  @item{Configuring the @secref{rpc-connect-content-type}}
  @item{Configuring the @secref{rpc-connect-x-api-key}}
  @item{Selecting the server's @seclink["rpc-connect-host-and-port-envvars"]{host and port}}
  @item{@seclink["rpc-connect-await-online"]{Waiting for the server to come online}}
  @item{@seclink["rpc-connect-http2"]{(Optional) use of HTTP/2}}
]

Since understanding each of these is a prerequisite to building @pyin{rpc} and
@pyin{rpc_callbacks} we'll begin with them first, then move onto a
@seclink["rpc-connect-js-lib-walkthrough"]{walkthrough} of how Reach's
JavaScript @tech{RPC} client library was constructed.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "rpc-connect-prereqs"]{Prerequisites}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-tls"]{TLS encryption}

It's the @tech{RPC} server's responsibility to acquire and configure the TLS
certs used to encrypt requests but it may still be necessary for the client to
perform some additional steps to make connections actually work.

The two main considerations are platform-specific setup to use HTTPS instead of
HTTP (different languages/stacks/request libraries have their own notions of
how this should function), and providing a "hook" to disable TLS hostname
verification (this is unsafe for production but often desirable during
development).

Libraries written by the Reach core developers team use environment variables
to @reachexlink[
  #:ref "35bf3d1"
  #:loc (cons "85" "92")
  "tut-6-rpc/docker-compose.yml"
  "achieve the latter:"]

@yaml{
  # examples/tut-6-rpc/docker-compose.yml

  - REACH_RPC_TLS_REJECT_UNVERIFIED=0 # Default "on"
  - NODE_TLS_REJECT_UNAUTHORIZED=0    # Default "on"
}

@envvar{NODE_TLS_REJECT_UNAUTHORIZED=0} is
@link["https://nodejs.org/api/cli.html#cli_node_tls_reject_unauthorized_value"]{baked into node.js}.

@envvar{REACH_RPC_TLS_REJECT_UNVERIFIED=0} is the convention we use for
languages which don't already have such a thing, and we recommend you use it in
your libraries as well.

@bold{Don't forget to alert users when TLS verification is disabled!}

@reachexlink[
  #:ref "1474d32"
  #:loc (cons "46" "52")
  "tut-6-rpc/client-go/index.go"
  "In Go, for example:"]

@go{
  skipVerify := os.Getenv("REACH_RPC_TLS_REJECT_UNVERIFIED") == "0"
  if skipVerify {
    fmt.Printf("\n*** Warning! TLS verification disabled! ***\n\n")
    fmt.Printf(" This is highly insecure in Real Life™ applications and must\n")
    fmt.Printf(" only be permitted under controlled conditions (such as\n")
    fmt.Printf(" during development).\n\n")
  }

  // ...jumping ahead to line 87:
  tls := &tls.Config { InsecureSkipVerify: skipVerify, }
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-content-type"]{@bold{Content-Type: application/json} HTTP header}

@tech{RPC} clients communicate with the server using JSON serialization for
data payloads. Configuring this header may or may not be automatic depending
upon your platform and which libraries you're using to manage network requests
over HTTPS.

For completeness' and correctness' sakes it's important that you ensure the
@bold{Content-Type} header is set to @bold{application/json; charset=utf-8}.

@reachexlink[
  #:ref "1474d32"
  #:loc "92"
  "tut-6-rpc/client-go/index.go"
  "In Go, for example:"]

@go{
  req.Header.Add("Content-Type", "application/json; charset=utf-8")
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-x-api-key"]{@bold{X-Api-Key} HTTP header}

The @tech{RPC} server only accepts requests from clients who include the
correct pre-shared API key in their headers. This key exists to prevent
unauthorized users from abusing the server if it were ever made accessible on
an untrusted network (e.g. the internet).

Make sure to set this header appropriately for consumers of your library.

@reachexlink[
  #:ref "1474d32"
  #:loc "93"
  "tut-6-rpc/client-go/index.go"
  "In Go, for example:"]

@go{
  key := os.Getenv("REACH_RPC_KEY")
  req.Header.Add("X-API-Key", key)
}

Here again we see the convention of using an environment variable to specify
this key's value. Libraries developed by the Reach core developers team call
this envvar @envvar{REACH_RPC_KEY} and we recommend you do the same for the
sake of consistency.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-host-and-port-envvars"]{Host and port envvars}

The last two environment variable conventions we suggest you observe are
@envvar{REACH_RPC_SERVER} and @envvar{REACH_RPC_PORT}. Unsurprisingly, these
are used to configure the host and port at which @tech{RPC} clients may connect
to a running @tech{RPC} server.

@reachexlink[
  #:ref "1474d32"
  #:loc "86"
  "tut-6-rpc/client-go/index.go"
  "In Go, for example:"]

@go{
  host := os.Getenv("REACH_RPC_SERVER")
  port := os.Getenv("REACH_RPC_PORT")
  uri  := fmt.Sprintf("https://%s:%s%s", host, port, m)
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-await-online"]{Waiting for server to come online}

There may be circumstances where @tech{RPC} clients come online before their
intended @tech{RPC} server is ready to serve requests. For this reason we
recommend you include a "wait for server" mechanism in your implementation of
@pyin{mk_rpc}.

@reachexlink[
  #:ref "1474d32"
  #:loc (cons "54" "71")
  "tut-6-rpc/client-go/index.go"
  "In Go, for example:"]

@go{
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
}

Notice that the client times out after five seconds rather than hanging
indefinitely.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-http2"]{Optionally using HTTP/2}

Finally, it's worth noting that Reach's @tech{RPC} server is capable of serving
requests via HTTP/2. Some HTTP client libraries will negotiate this connection
for you transparently with no code changes; others may require manual steps.

If you run into trouble with auto-negotiation of HTTP versions or see odd
errors having to do with @link["https://tools.ietf.org/html/rfc7301"]{ALPN} you
may have better luck dropping down to HTTP/1.1 instead (which works because the
server is backwards-compatible).


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "rpc-connect-js-lib-walkthrough"]{JavaScript RPC client library walkthrough}

Now we've covered the necessary peripheral nuts and bolts it's time to pull
everything together into a cohesive whole.

@;TODO consider using `reachex` and:
  @;TODO * add git ref support
  @;TODO * fix/figure out correct #:dir overrides + GitHub linking

We'll be using Reach's JavaScript @tech{RPC} client library as a reference
implementation for your own work. Here it is in
@reachexlink[
  #:ref "9d4618f"
  #:dir "js"
  "rpc-client/index.mjs"
  "its entirety:"]

@;TODO move `rpcReady` invocation inside `mkRPC`
@js{
  import waitPort from 'wait-port';
  import bent     from 'bent';

  export const mkRPC = opts => {
    const defaults = {
      host:   'REACH_RPC_SERVER',
      port:   'REACH_RPC_PORT',
      apiKey: 'REACH_RPC_KEY',
    };

    const r = (acc, k) =>
      Object.assign(acc, { [k]: process.env[defaults[k]] });

    const o = Object.assign(
      Object.keys(defaults).reduce(r, {}),
      opts);

    Object.keys(o).forEach(k => {
      if (!o[k]) {
        throw new Error(`Neither \`opts.${k}\` nor ${defaults[k]} environment`
                      + ` variable are configured!`);
      }
    });

    const call = bent(`https://${o.host}:${o.port}`, `POST`, `json`, 200, {
      'X-API-Key': o.apiKey,
    });

    const rpcReady = async () => {
      await waitPort({ host: o.host, port: parseInt(o.port, 10) });
    };

    const rpc = async (m, ...args) => {
      const lab = `RPC ${m} ${JSON.stringify(args)}`
      console.log(`${lab}`);
      const ans = await call(m, args);
      console.log(`${lab} ==> ${JSON.stringify(ans)}`);
      return ans;
    };

    const rpcCallbacks = async (m, arg, cbacks) => {
      const vals = {};
      const meths = {};
      for (const k in cbacks) {
        const v = cbacks[k];
        if ( v instanceof Function ) {
          meths[k] = true;
        } else {
          vals[k] = v;
        }
      }
      return new Promise((resolve, reject) => (async () => {
        let p = rpc(m, arg, vals, meths);
        while (true) {
          try {
            const r = await p;
            switch ( r.t ) {
              case 'Done': {
                return resolve(r.ans);
              }
              case 'Kont': {
                const { kid, m, args } = r;
                const ans = await cbacks[m](...args);
                p = rpc(`/kont`, kid, ans);
                break;
              }
              default:
                throw new Error(`Illegal callback return: ${JSON.stringify(r)}`);
            }
          } catch (e) {
            return reject(e);
          }
        }
      })());
    };

    return { rpc, rpcReady, rpcCallbacks };
  };
}

Let's break this down.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-js-lib-walkthrough-imports"]{Imports}

First our imports:

@js{
  import waitPort from 'wait-port';
  import bent     from 'bent';
}

@jsin{waitPort} is how our client will pause and wait for the server to become
available. If there isn't already a canned solution to this problem in your
language it's simple to
@seclink["rpc-connect-await-online"]{implement one yourself}.

@jsin{bent} handles our HTTPS requests, as you'll see in a moment.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-js-lib-walkthrough-options"]{Options}

Next some admin:

@js{
  export const mkRPC = opts => {
    const defaults = {
      host:   'REACH_RPC_SERVER',
      port:   'REACH_RPC_PORT',
      apiKey: 'REACH_RPC_KEY',
    };

    const r = (acc, k) =>
      Object.assign(acc, { [k]: process.env[defaults[k]] });

    const o = Object.assign(
      Object.keys(defaults).reduce(r, {}),
      opts);

    Object.keys(o).forEach(k => {
      if (!o[k]) {
        throw new Error(`Neither \`opts.${k}\` nor ${defaults[k]} environment`
                      + ` variable are configured!`);
      }
    });

  ...
  };
}

The client defaults to using the @envvar{REACH_RPC_<OPTION NAME>} environment
variables mentioned earlier but allows consuming code to selectively override
these values. If neither an @jsin{opts['<option name>']} nor corresponding
environment variable has been populated then an exception is thrown and the
library-user will be unable to continue.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-js-lib-walkthrough-post"]{HTTP POST}

Next we'll create a shorthand for actually sending @tech{RPC} requests.

@js{
  const call = bent(`https://${o.host}:${o.port}`, `POST`, `json`, 200, {
    'X-API-Key': o.apiKey,
  });
}

@jsin{call} plugs into the JavaScript @jsin{bent} library and sets up a
reusable JSON POST operation which speaks to the correct @jsin{host}, on the
correct @jsin{port}, and with the correct @jsin{apiKey} appended to its
headers.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-js-lib-walkthrough-rpc"]{The @bold{rpc} function}

Now we'll construct the workhorse of our client: @jsin{rpc}.

@js{
  const rpc = async (m, ...args) => {
    const lab = `RPC ${m} ${JSON.stringify(args)}`
    console.log(`${lab}`);
    const ans = await call(m, args);
    console.log(`${lab} ==> ${JSON.stringify(ans)}`);
    return ans;
  };
}

This simple function logs the request it's about to make, makes it, then logs
and returns the response.

Note the function arguments: @jsin{m} represents the @tech{RPC} method (e.g.
@urlin{/stdlib/balanceOf}), and @jsin{...args} makes use of JavaScript's
support of
@link["https://en.wikipedia.org/wiki/Variadic_function#In_JavaScript"]{variadic functions}
to capture arguments which are then forwarded to the operation represented by
@jsin{m}.

If your language doesn't support variadics the solution is simple: just make
the second argument a list-type!


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[
  #:tag "rpc-connect-js-lib-walkthrough-rpc-callbacks"
  ]{The @bold{rpcCallbacks} function}

The @jsin{rpcCallbacks} function is somewhat more involved.

@js{
  const rpcCallbacks = async (m, arg, cbacks) => {
    const vals = {};
    const meths = {};
    for (const k in cbacks) {
      const v = cbacks[k];
      if ( v instanceof Function ) {
        meths[k] = true;
      } else {
        vals[k] = v;
      }
    }
    return new Promise((resolve, reject) => (async () => {
      let p = rpc(m, arg, vals, meths);
      while (true) {
        try {
          const r = await p;
          switch ( r.t ) {
            case 'Done': {
              return resolve(r.ans);
            }
            case 'Kont': {
              const { kid, m, args } = r;
              const ans = await cbacks[m](...args);
              p = rpc(`/kont`, kid, ans);
              break;
            }
            default:
              throw new Error(`Illegal callback return: ${JSON.stringify(r)}`);
          }
        } catch (e) {
          return reject(e);
        }
      }
    })());
  };
}

We begin by bisecting the @jsin{cbacks} argument into a hash of values and a
hash of methods. This effectively splits static fields (@jsin{vals}) in a
@tech{participant interact interface} from callable functions (@jsin{meths}).

Then we spawn an asynchronous task in the form of a @jsin{Promise} which will
advance the state of the @DApp by continuously invoking PII functions until
the server responds with a message indicating completion.

In the first @tech{RPC} request, @jsin{m} will be a call to a method such as
@urlin{/backend/Alice} or @urlin{/backend/AnotherParticipant} which begins the
cycle.

Subsequent requests are directed to the @urlin{/kont} method (which is another
name for continuation). This is the only "semi-magical" ingredient in our
@tech{RPC} protocol; it's this step which allows asynchronous processes
controlled by the server to pause and await further input from the client(s)
before resuming.

In each iteration of our @jsin{while} loop we reset @jsin{p} (the request), and
await @jsin{r} (the response).

If the @jsin{r.t} field indicates that we're done we'll return the @jsin{r.ans}
answer to the consuming code which originally invoked @jsin{rpcCallbacks}.

If we're not done yet and @jsin{r.t} indicates we should perform a "Kont" step,
then from @jsin{r} we grab the @jsin{kid} continuation ID, @jsin{m} method name
of the PII, and the @jsin{args} we'll supply to the method @jsin{m} indexes.

Having computed an @jsin{ans} to feed into the server's @jsin{kid} continuation,
we POST the request and continue the cycle.

If @jsin{r.t} equals anything other than "Done" or "Kont" we raise an exception.

That's it!


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "rpc-connect-js-lib-walkthrough-mk-rpc"]{@bold{mkRPC}}

The final step is to simply return the @jsin{rpc} and @jsin{rpcCallbacks}
functions we've created so our users can call them from their @tech{frontends}.

@js{
  return { rpc, rpcReady, rpcCallbacks };
}


@;TODO * Summarize/finish somehow less abruptly
@;TODO * @jsin{...} sometimes causes .badlink CSS highlighting
@;TODO * Pick between "route" vs. "method"
