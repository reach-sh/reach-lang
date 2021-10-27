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
        var startingBalance = await rpc.CallAsync("/stdlib/parseCurrency", 100);
        var accAlice        = await rpc.CallAsync("/stdlib/newTestAccount", startingBalance);
        var accBob          = await rpc.CallAsync("/stdlib/newTestAccount", startingBalance);
        
        var beforeAlice     = await rpc.CallAsync("/stdlib/getBalance", accAlice);
        var beforeBob       = await rpc.CallAsync("/stdlib/getBalance", accBob);

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
  
     static void Main()
      {
        Program p = new Program();
        p.GetAcc().Wait();
        
         }


    }

