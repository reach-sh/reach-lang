using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.IO;
using System.Threading.Tasks;

namespace Rpc
{

    public class RPCOptions
{
    public string host;
    public string port;
    public string key;
    public int timeout = 5;
    public string verify = "0";

    public RPCOptions(string host, string port, string key)
    {
        this.host = host;
        this.port = port;
        this.key = key;
    }
    public RPCOptions(string host, string port, string key, int timeout, string verify)
    {
        this.host = host;
        this.port = port;
        this.key = key;
        this.timeout = timeout;
        this.verify = verify;
    }
}

public class RPCValue
{
    public string name;
    public dynamic value;
    public RPCValue(string argName, dynamic val)
    {
        this.name = argName;
        this.value = val;
    }
}

public class RPCCallback
{
    public string name;
    public dynamic value;
    public RPCCallback(string argName, Func<dynamic, dynamic> callback)
    {
        this.name = argName;
        this.value = callback;
    }

    public RPCCallback(string argName, Func<dynamic, Task<dynamic>> callback)
    {
        this.name = argName;
        this.value = callback;
    }
}

[Serializable]
public class CallbackResponse
{
    public string m;
    public string t;
    public string kid;
}
public interface ILogger //public added
    {
        void Info(string message);
        void Error(string message);
    }

    public class Logger : ILogger
    {
        public void Error(string message)
        {
            Console.WriteLine($"Error: {message}");
        }

        public void Info(string message)
        {
            Console.WriteLine($"Info: {message}");
        }
    }
public class RPCMultiTyped
{
    private string rawValue;
    private RPCOptions options;
    private ReachRPC reachRPC;

    public RPCMultiTyped(string value, RPCOptions opts)
    {
        this.rawValue = value;
        this.options = opts;
        this.reachRPC = new ReachRPC(opts);
    }

    public string AsString()
    {
        return rawValue;
    }

    public int AsInt()
    {
        return Int32.Parse(rawValue);
    }

    public bool AsBool()
    {
        return Boolean.Parse(rawValue);
    }
    public RPCMultiTyped[] AsArray()
    {
        string[] r1 = RPCUtils.ExtractArray(rawValue);
        RPCMultiTyped[] r2 = new RPCMultiTyped[r1.Length];

        for (int i = 0; i < r2.Length; i++)
        {
            r2[i] = new RPCMultiTyped(r1[i], options);
        }

        return r2;
    }

    async public Task<string> AsFormattedCurrency(int precision)
    {
        return await reachRPC.CallAsync("/stdlib/formatCurrency", rawValue, precision);
    }
}

public class ReachRPC
{
    private RPCOptions options;

    public ReachRPC(RPCOptions opts)
    {
        options = opts;
    }
        public string StringifyArgs(object[] args)
    {
        string data = "[";

        for (int i = 0; i < args.Length; i++)
        {
            data = String.Concat(data, args[i], i == args.Length - 1 ? "" : ", ");
        }

        data += "]";

        return data;
    }
    static readonly HttpClient client = new HttpClient();

    public async Task<string> CallAsync(string path, params object[] args)
    {
        ILogger logger = new Logger();

        logger.Info("Program has startedzzzz");
        string url = "https://" + options.host + ":" + options.port + path;
        string postData = args.Length > 0 ? StringifyArgs(args) : "";
        var httpWebRequest = WebRequest.CreateHttp(url);
        httpWebRequest.ContentType = "application/json";

                Console.WriteLine("\nThe HttpHeaders are \n{0}",httpWebRequest.Headers);

                httpWebRequest.Method = "POST";    

                var httpResponse =  (HttpWebResponse)httpWebRequest.GetResponse();
                using (var streamReader = new StreamReader(httpResponse.GetResponseStream()))
                {
                    var responseText = streamReader.ReadToEnd();
                    logger.Info(responseText);
                } 
        try
         {
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);

         }
        catch (HttpRequestException e)
         {
                 logger.Error(e.Message);
         }
        await Task.Yield();
        return postData;
    }
 public async Task Callbacks(string path, string contract, RPCValue[] values, RPCCallback[] callbacks)
    {
        string valueString = "{ ";

        string callbackString = "{ ";
        for (int i = 0; i < callbacks.Length; i++)
            callbackString += "\"" + callbacks[i].name + "\": true" + (i == callbacks.Length - 1 ? "" : ", ");
        callbackString += " }";

        string bodyString = contract + ", " + valueString + ", " + callbackString;

        var response = await CallAsync(path, bodyString);
    }
    
}

public class RPCUtils
{
    public static string[] ExtractArray(string value)
    {
        string t1 = value.Remove(0, 1);
        string t2 = t1.Remove(t1.Length - 1, 1);
        string t3 = t2.Trim();
        if (t3.Length == 0)
            return null;

        List<string> extArgs = new List<string>();

        string nextString = "";
        List<char> targets = new List<char>() { '\0' };
        int i = 0;
        while (i < t3.Length)
        {
            char currentChar = t3[i];

            if (currentChar == '{' || currentChar == '[')
            {
                Console.WriteLine("Saw " + currentChar);
                nextString += currentChar;
                targets.Add(currentChar == '{' ? '}' : ']');
                i++;
                continue;
            }
            else if (targets.Count > 1 && currentChar == targets[targets.Count - 1])
            {
                Console.WriteLine("Removing " + currentChar);
                nextString += currentChar;
                targets.RemoveAt(targets.Count - 1);
                i++;
                continue;
            }
            else if (targets.Count == 1 && currentChar == ',')
            {
                Console.WriteLine("Saw , with no target");
                extArgs.Add(nextString.Trim());
                nextString = "";
                i++;
                continue;
            }
            nextString += currentChar;
            i++;
        }
        extArgs.Add(nextString.Trim());

        return extArgs.ToArray();
    }

}
}