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
    public readonly string val;
    public Value(string _id, string _val) {
      id = _id;
      val = _val;
    }
    override public string AsJson() {
      return "\"" + id + "\"" + ":" + val;
    }
  }

  public class Method : Callback {
    public readonly string id;
    public readonly Func<string[], Task<string>> impl;
    public Method(string _id, Func<string[], Task<string>> _impl) {
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
    public void Value(string id, string val) {
      _vs.Add(new Value(id, val));
    }
    public void Method(string id, Func<string[], Task<string>> impl) {
      _ms.Add(new Method(id, impl));
    }
    public void Methods(string id) {
      _vs.Add(new Value(id, "true"));
    }

    public async Task<string> Call(string target, string[] args) {
      foreach (Method m in _ms) {
        if (m.id == target) {
          return await m.impl(args);
        }
      }
      throw new InvalidOperationException($"Unknown method {target}");
    }

    public string AsJson() {
      var vs = "{" + string.Join(",", _vs.Select(x => x.AsJson())) + "}";
      var ms = "{" + string.Join(",", _ms.Select(x => x.AsJson())) + "}";
      return vs + "," + ms;
    }
  }

  public class CallbackResponse {
    public string m;
    public string t;
    public JsonElement kid;
    public JsonElement[] args;
  }

  public interface ILogger {
    void Info(string message);
    void Error(string message);
  }
  public class Logger : ILogger {
    public void Error(string message) {
      Console.WriteLine($"Error: {message}");
    }
    public void Info(string message) {
      Console.WriteLine($"Info: {message}");
    }
  }

  public class Client {
    private readonly HttpClient client;
    private readonly ILogger logger;
    private readonly TimeSpan timeout;
    private bool repliedOnce = false;
    public Client(Options opts) {
      logger = new Logger();

      if ( opts.verify ) {
        client = new HttpClient();
      } else {
        logger.Info("*** Warning! TLS verification disabled! ***");
        logger.Info(" This is highly insecure in Real Life applications and must");
        logger.Info(" only be permitted under controlled conditions (such as");
        logger.Info(" during development)...");
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
    public async Task<string> Call(string path, params string[] args) {
      var data = "[" + string.Join(",", args) + "]";
      var content = new StringContent(data, Encoding.UTF8, "application/json");
      var cts = new CancellationTokenSource();
      if ( ! repliedOnce ) {
        cts.CancelAfter(timeout);
      }
      var response = await client.PostAsync(path, content, cts.Token);
      response.EnsureSuccessStatusCode();
      var rs = await response.Content.ReadAsStringAsync();
      repliedOnce = true;
      return rs;
    }
    public async Task Callbacks(string path, string ctc, Callbacks cbs) {
      var rs = await Call(path, ctc, cbs.AsJson());
      var opts = new JsonSerializerOptions{
        IncludeFields = true,
      };
      while ( true ) {
        logger.Info($"Callback: {rs}");
        var cbr = JsonSerializer.Deserialize<CallbackResponse>(rs, opts);
        if ( cbr.t == "Done" ) { return; }
        logger.Info($"Callback: {rs} => {cbr.m}");
        string[] args = cbr.args.Select(x => x.GetRawText()).ToArray();
        var mrng = await cbs.Call(cbr.m, args);
        logger.Info($"Callback: {rs} => {cbr.m} => {mrng}");
        rs = await Call("/kont", cbr.kid.GetRawText(), mrng);
      }
    }
  }
}
