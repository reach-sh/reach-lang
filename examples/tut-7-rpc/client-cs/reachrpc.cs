using System;
using System.Web;
using System.Threading.Tasks;
using Rpc;
 public class Program
    {

public async Task<string> GetAcc()
{
  try 
  {
        RPCOptions opts = new RPCOptions("127.0.0.1", "3000", "opensesame");
        ReachRPC rpc;
        rpc = new ReachRPC(opts); 
        var balanceRequest = await rpc.CallAsync("/stdlib/parseCurrency", 100);
        string startingBalance = balanceRequest.ToString();

        var accountRequest = await rpc.CallAsync("stdlib/newTestAccount", startingBalance);
        string account = accountRequest.ToString();
        return account;
      //  var startingBalance = await rpc.CallAsync("/stdlib/parseCurrency", 100);

        var accAlice        = await rpc.CallAsync("/stdlib/newTestAccount", startingBalance);
        var accBob          = await rpc.CallAsync("/stdlib/newTestAccount", startingBalance);
        var ctcAlice        = rpc.CallAsync("/acc/deploy",  accAlice).ToString();

        string[] HAND;
        string[] OUTCOME;
        HAND = new string[3]{"Rock", "Paper", "Scissors"};
        OUTCOME = new String[3]{"Win", "Lose", "Draw"};
       // var player = new Player(HAND, OUTCOME);
       return ctcAlice;
  }
  catch (Exception e)
  {
    Console.WriteLine(e.Message);
    return null;
  
  }
  
}
private async Task<string> Deploy(string account)
{
  try 
  {
        RPCOptions opts = new RPCOptions("127.0.0.1", "3000", "opensesame");
        ReachRPC rpc;
        rpc = new ReachRPC(opts);
        var contractRequest = await rpc.CallAsync("/acc/deploy", account);
        string contract = contractRequest.ToString();
        return contract;
  }
  catch (Exception e)
  {
    Console.WriteLine(e.Message);
    return null;
  
  }
}
private async Task<string> Attach(string account)
{
  try
  {
    RPCOptions opts = new RPCOptions("127.0.0.1", "3000", "opensesame");
    ReachRPC rpc;
    rpc = new ReachRPC(opts);
    var contractRequest = await rpc.CallAsync("/acc/attach", account);
    string contract = contractRequest.ToString();
    return contract;

  }
  catch (Exception e)
  {
    Console.WriteLine(e.Message);
    return null;

  }
}
private async Task<string> GetContract()
{
  string account = await GetAcc();
  string contract;
  contract = await Deploy(account);
  return contract;
}
private async Task<string> Connect()
{
  string contract = await GetContract();
 // var con = await Play();
  return contract;
}
/*
 private async Task<string> Play()
{
 RPCOptions opts = new RPCOptions("127.0.0.1", "3000", "opensesame");
        ReachRPC rpc;
        rpc = new ReachRPC(opts);
        
          var player = await rpc.Callbacks( "/backend/Alice");
return player;
}
*/
     static void Main()
      {
        Program p = new Program();
        p.GetAcc().Wait();
        //    client.Disconnect();
     //   p.Play().Wait();
        
         }


    }

