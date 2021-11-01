using System;
using System.Web;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Text;
using System.Text.Json;
using System.Linq;

namespace Reach.RPC {
  public class Options {
    public Options() {}

    private string _host;
    public string host {
      get {
        if ( _host != null ) { return _host; }
        return Environment.GetEnvironmentVariable("REACH_RPC_SERVER");
      }
      set { _host = value; }
    }

    private int? _port;
    public int port {
      get {
        if ( _port != null ) { return (int)_port; }
        var x = Environment.GetEnvironmentVariable("REACH_RPC_PORT");
        return Int32.Parse(x);
      }
      set { _port = value; }
    }

    private bool? _verify;
    public bool verify {
      get {
        if ( _verify != null ) { return (bool)_verify; }
        var x = Environment.GetEnvironmentVariable("REACH_RPC_TLS_REJECT_UNVERIFIED");
        return (x != "0");
      }
      set { _verify = value; }
    }

    private int? _timeout;
    public int timeout {
      get {
        if ( _timeout != null ) { return (int)_timeout; }
        var x = Environment.GetEnvironmentVariable("REACH_RPC_TIMEOUT");
        if ( x == null || x == "" ) { return 5; }
        return Int32.Parse(x);
      }
      set { _timeout = value; }
    }

    private string _key;
    public string key {
      get {
        if ( _key != null ) { return _key; }
        return Environment.GetEnvironmentVariable("REACH_RPC_KEY");
      }
      set { _key = value; }
    }

  }

  abstract public class Callback {
    abstract public string AsJson();
  }

  public class Value : Callback {
    public readonly string id;
    public readonly JsonElement val;
    public Value(string _id, JsonElement _val) {
      id = _id;
      val = _val;
    }
    override public string AsJson() {
      return "\"" + id + "\"" + ":" + val.GetRawText();
    }
  }

  public class Method : Callback {
    public readonly string id;
    public readonly Func<JsonElement[], Task<JsonElement>> impl;
    public Method(string _id, Func<JsonElement[], Task<JsonElement>> _impl) {
      id = _id;
      impl = _impl;
    }
    override public string AsJson() {
      return "\"" + id + "\"" + ":" + "true";
    }
  }

  public class Callbacks {
    private readonly List<Value> _vs;
    private readonly List<Method> _ms;
    public Callbacks() {
      _vs = new List<Value>();
      _ms = new List<Method>();
    }
    public void Value(string id, JsonElement val) {
      _vs.Add(new Value(id, val));
    }
    public void Method(string id, Func<JsonElement[], Task<JsonElement>> impl) {
      _ms.Add(new Method(id, impl));
    }
    public void Methods(string id) {
      _vs.Add(new Value(id, Client.AsJson("true")));
    }

    public async Task<JsonElement> Call(string target, JsonElement[] args) {
      foreach (Method m in _ms) {
        if (m.id == target) {
          return await m.impl(args);
        }
      }
      throw new InvalidOperationException($"Unknown method {target}");
    }

    public JsonElement ValuesAsJson() {
      return Client.AsJson("{" + string.Join(",", _vs.Select(x => x.AsJson())) + "}");
    }
    public JsonElement MethodsAsJson() {
      return Client.AsJson("{" + string.Join(",", _ms.Select(x => x.AsJson())) + "}");
    }
  }

  public class CallbackResponse {
    public string m;
    public string t;
    public JsonElement kid;
    public JsonElement[] args;
  }

  public class Client {
    static public JsonElement AsJson(string s) {
      return JsonSerializer.Deserialize<JsonElement>(s);
    }

    private readonly HttpClient client;
    private readonly TimeSpan timeout;
    private bool repliedOnce = false;
    public Client(Options opts) {
      if ( opts.verify ) {
        client = new HttpClient();
      } else {
        Console.Error.WriteLine("*** Warning! TLS verification disabled! ***");
        Console.Error.WriteLine(" This is highly insecure in Real Life applications and must");
        Console.Error.WriteLine(" only be permitted under controlled conditions (such as");
        Console.Error.WriteLine(" during development)...");
        var handler = new HttpClientHandler() {
          ServerCertificateCustomValidationCallback = delegate { return true; },
        };
        client = new HttpClient(handler);
      }

      client.BaseAddress = new Uri("https://" + opts.host + ":" + opts.port);
      timeout = TimeSpan.FromSeconds(opts.timeout);
      client.DefaultRequestHeaders.Add("X-API-Key", opts.key);
      client.DefaultRequestHeaders.Add("cache-control", "no-cache");
    }
    public async Task<JsonElement> Call(string path, params JsonElement[] args) {
      var data = "[" + string.Join(",", args.Select((x) => x.GetRawText())) + "]";
      var content = new StringContent(data, Encoding.UTF8, "application/json");
      var cts = new CancellationTokenSource();
      if ( ! repliedOnce ) {
        cts.CancelAfter(timeout);
      }
      var response = await client.PostAsync(path, content, cts.Token);
      response.EnsureSuccessStatusCode();
      var rs = await response.Content.ReadAsStringAsync();
      repliedOnce = true;
      return Client.AsJson(rs);
    }
    public async Task Callbacks(string path, JsonElement ctc, Callbacks cbs) {
      var rs = await Call(path, ctc, cbs.ValuesAsJson(), cbs.MethodsAsJson());
      var opts = new JsonSerializerOptions{
        IncludeFields = true,
      };
      while ( true ) {
        var cbr = JsonSerializer.Deserialize<CallbackResponse>(rs, opts);
        if ( cbr.t == "Done" ) { return; }
        var mrng = await cbs.Call(cbr.m, cbr.args);
        rs = await Call("/kont", cbr.kid, mrng);
      }
    }
  }
}
