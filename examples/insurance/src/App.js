import React, { useState, useEffect, useRef } from 'react';
import './App.css';
import * as backend from './reach-build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
import { ALGO_WalletConnect as WalletConnect } from '@reach-sh/stdlib';
import { createClient } from "@supabase/supabase-js";
import useConfirm from "./hooks/useConfirm";
import Dashboard from './dashbord';
import Connect from './components/connect';
import SignupForm from './components/signup-form';
import Deployer from './components/deployer';
import ErrorPage from './components/error';

const reach = loadStdlib({
  REACH_CONNECTOR_MODE: "ALGO-browser",
  PUBLIC_URL: "https%3A%2F%2Fr.bridge.walletconnect.org"
});

reach.setWalletFallback(reach.walletFallback({
  providerEnv: {
    ALGO_TOKEN: '',
    ALGO_SERVER: "https://testnet-api.algonode.cloud",
    ALGO_PORT: '',
    ALGO_INDEXER_TOKEN: '',
    ALGO_INDEXER_SERVER: "https://testnet-idx.algonode.cloud",
    ALGO_INDEXER_PORT: '',
  },
  WalletConnect
}));

const SUPABASE_URL = "https://byolfysahovehogqdena.supabase.co";
const SUPABASE_ANON_KEY = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ5b2xmeXNhaG92ZWhvZ3FkZW5hIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDg3NTcwMTYsImV4cCI6MTk2NDMzMzAxNn0.Q5h8nwP-qy1o5oDa0UCAgj1m7vTXOlhPyoZRC-0CNnk";
const supabaseClient = createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

function App() {
  const { confirm } = useConfirm();
  const [communityGroupName, setCommunityGroupName] = useState("Test group insurance");
  const [mandatoryEntryFee, setMandatoryEntryFee] = useState(0);
  const insurerContract = useRef(null);
  const currentUser = useRef({ fullName: "Guest" });
  const contractInfo = useState({});
  const algoAccount = useRef({ networkAccount: { addr: "" } });
  const mnemonicRef = useRef(<></>);
  const [mnemonicStr, setMnemonicStr] = useState("");
  const [loginErr, setLoginErr] = useState("");
  const [activePage, setActivePage] = useState("LOGIN");
  const [deployed, setDeployed] = useState(false);
  const [isSavingContractInfo, setIsSavingContractInfo] = useState(false);
  const [contractInfoSaved, setContractInfoSaved] = useState(false);
  const [errMessage, setErrMessage] = useState("Err occured.");
  const [errCode, setErrCode] = useState("GOTO_LOGIN");
  const [deployerModeOn, setDeployerModeOn] = useState(false);
  const [connecting, setConnecting] = useState(false);
  const [refreshCount, setRefreshCount] = useState(0);
  const interact = useRef({
    communityGroupName: communityGroupName,
    mandatoryEntryFee: Number(mandatoryEntryFee),
    contractIsRunning: true,
    saveNewMemberDetails: async ({ fullName, phone, email, chosenInsurancePackage }) => {
      const { data, error } = await supabaseClient.from("members").insert([{
        fullName, phone, email, chosenInsurancePackage, memberAddr: algoAccount.current.networkAccount.addr
      }]);
      if (error) {
        console.log(`Error while saving member details ${error}`);
      } else {
        console.log(`Member registered successfully: ${JSON.stringify(data)}`);
      }
    },
    seeFeedback: () => {
      console.log("insurer saw feedback on deploying the contract");
      return;
    },
    saveNewClaim: async ({ amountRequested, description }) => {
      const deadline = new Date();
      deadline.setDate(deadline.getDate() + 10);

      const amountSet = amountRequested;
      const sumOfSetAmounts = 0;
      const approvalscount = 0;
      const addr = algoAccount.current.networkAccount.addr;
      const { data: newClaimData, error } = await supabaseClient.from("claims").insert([{
        claimant: addr,
        amountRequested, amountSet, sumOfSetAmounts,
        approvalscount, description, deadline
      }]);
      if (error) {
        console.log(`Error while saving new claim details `, error);
      } else {
        if (newClaimData.length > 0) {
          const claimId = newClaimData[0].id;
          const { data: members } = await supabaseClient.from("members").select("memberAddr");
          members.forEach(async ({ memberAddr: notified }) => {
            const { error } = await supabaseClient.from("claimnotifications").insert([{ claimId: claimId, member: notified, claimant: addr }]);
            if (error) { console.log(error); }
          });
        }
        console.log(`New claim recorded successfully: ${JSON.stringify(newClaimData)}`);
      }
    },
    createInvoices: () => {
      console.log("creating invoices at the end of the month ...");
    },
    moveMaturedPayments: () => {
      console.log("moving matured payments from temporary queue ...");
    },
    getMemberData: async () => {
      const memberAddress = algoAccount.current.networkAccount.addr;
      const { data: memberDataArr, error } = await supabaseClient.from("members").select("*").eq('memberAddr', memberAddress);
      if (error) {
        console.log("interact.getMemberDetails errored: ", error.message);
      }
      if (memberDataArr.length > 0) {
        const { matureBalance, fundLimit, chosenInsurancePackage, amountDue } = memberDataArr[0];
        return {
          insrPackageId: chosenInsurancePackage,
          amountDue,
          matureBalance,
          fundLimit
        };
      } else {
        return {
          insrPackageId: 1,
          amountDue: 0,
          matureBalance: 0,
          fundLimit: 0
        };
      }
    },
    stopContract: async () => {
      interact.current.contractIsRunning = false;
      await insurerContract.current.p.Insurer(interact.current);
      //delete info record from supabase
      const { error: err } = await supabaseClient.from("smartcontracts").delete().match({ name: "insurancedapp" });
      if (err) { console.log("err: ", err); }
    },
    signout: () => {
      console.log("signing out of contract, leaving it running");
    },
    notifyFundedMember: async (address) => {
      const { data, error } = await supabase.from('claims').update({ status: "funded" }).match({ claimant: address });
      console.log(`Member address ${address} funded.`);
    },
    log: (msg) => {
        console.log(msg);
    }
  });

  //============
  useEffect(() => {
    setConnecting(true);
    async function readFromDb() {
      try {
        //fetch the contract info
        const { data: infoArr, error } = await supabaseClient.from("smartcontracts").select("info").eq('name', "insurancedapp");
        if (error) { setConnecting(false); throw error; }
        //if info was found, 
        if (infoArr.length > 0) {
          setDeployed(true);
          contractInfo.current = JSON.parse(infoArr[0].info);
          console.log("contract info found: ", infoArr[0].info);
        }
        setConnecting(false);
      } catch (er) {
        console.log("Oops! Failed to fetch the contract info/address", er);
      }
    }

    readFromDb();
  }, []);

  //==========
  function LoginWithMnemonic(e) {
    ConnectWallet(e, true);
  }
  function ConnectWallet(e, loginWithMnemonic = false) {
    e.preventDefault();
    if (connecting) {
      alert("Pease wait (Page is still loading)");
    } else {
      try {
        console.log("ConnectWallet(){...}");
        
        let getPromise = null;
        if (loginWithMnemonic) {
          getPromise = reach.newAccountFromMnemonic(mnemonicStr);
        } else {
          getPromise = reach.getDefaultAccount();
        }

        getPromise.then((acc) => {
          setLoginErr("");
          algoAccount.current = acc;
          
          if (!deployed) {
            //https://devrecipes.net/custom-confirm-dialog-with-react-hooks-and-the-context-api/
            console.log("Awaiting want to deploy prompt");
            confirm('The insurer contract is not yet deployed. Do you want to deploy it ?.')
              .then(wantToDeployContract => {
                if (wantToDeployContract) {
                  setActivePage("DEPLOYER");
                } else {
                  setErrMessage("Wait for the insurer to deploy the contract, or contact them for help");
                  setErrCode("GOTO_LOGIN");
                  setActivePage("ERROR");
                }
              });
          } else if (deployerModeOn) {
            setActivePage("DEPLOYER");
          } else {
            async function accessDb() {
              let isRegisteredMember = false;
              const memberAddr = algoAccount.current.networkAccount.addr;
              const { data: memberDataArr, error } = await supabaseClient.from("members").select("*").eq('memberAddr', memberAddr);
              if (error) {
                setErrMessage(error.message);
                setErrCode("GOTO_LOGIN");
                setActivePage("ERROR");
              }
              if (memberDataArr.length > 0) {
                isRegisteredMember = true;
                currentUser.current = memberDataArr[0];
              }
              if (isRegisteredMember) {
                //create a contract handle and assign it to insurerContract
                insurerContract.current = algoAccount.current.contract(backend, contractInfo.current);
                
                setActivePage("DASHBOARD");
              } else {
                setActivePage("SIGNUP");
              }
            }
            accessDb();
          }
        }).catch((er) => {
          setLoginErr(er.message);
        });

      } catch (er) {
        console.log("er: ", er);
      }
    }
  }

  //=======
  const deployContract = async (e) => {
    e.preventDefault();
    setIsSavingContractInfo(true);

    const insurerAccount = algoAccount.current;
    //deploy the contract now
    const ctc = insurerAccount.contract(backend);
    insurerContract.current = ctc;
    
    // Set the deployed contract's initial state
    insurerContract.current.p.Insurer(interact.current);
    
    //getting the contract info
    insurerContract.current.getInfo().then(async (info) => {
        const infoStr = JSON.stringify(info);
        
        //save the contract info into supabase
        const { data, error: err } = await supabaseClient.from("smartcontracts").insert([{
          name: "insurancedapp", info: infoStr
        }]);
        setIsSavingContractInfo(false);
        if (err) {
          console.log(`Error while saving the contract info to supabase ${err}`);
        } else {
          console.log(`Saved contract info to supabase: ${JSON.stringify(data)}`);
          setDeployed(true);
        }
    });
    
    //deployer will leave the contract running, signout from it
    await reach.withDisconnect(() => {
        reach.disconnect(null);
    });
    setContractInfoSaved(true);
  };

  //deployer may later decide to stop th contract, but will attach as a special member
  const stopContract = async () => {
    const insurerAccount = algoAccount.current;
    const insurerContractHandle = insurerAccount.contract(backend, contractInfo.current);
    const ok = await insurerContractHandle.apis.CommunityMember.stopContract();
    setContractInfoSaved(!ok);
  };

  function refreshDashbord() {
    setRefreshCount(refreshCount + 1);
  }

  return (
    <>
      {(activePage === "LOGIN") ?
        
        <Connect
           setDeployerModeOn={setDeployerModeOn} 
           deployerModeOn={deployerModeOn} 
           loginErr={loginErr} 
           ConnectWallet={ConnectWallet} 
           connecting={connecting}
           mnemonicStr={mnemonicStr} 
           mnemonicRef={mnemonicRef}
           setMnemonicStr={setMnemonicStr}
           LoginWithMnemonic={LoginWithMnemonic}
        />

        : (activePage === "SIGNUP") ?

          <SignupForm
            algoAccount={algoAccount}
            setActivePage={setActivePage}
            backend={backend}
            contractInfo={contractInfo}
          />

          : (activePage === "DASHBOARD") ?

            <Dashboard
              insurerContract={insurerContract.current}
              addr={algoAccount.current.networkAccount.addr}
              algoAccount={algoAccount}
              backend={backend}
              contractInfo={contractInfo}
              currentUser={currentUser.current}
              refreshDashbord={refreshDashbord}
            />

            : (activePage === "DEPLOYER") ?
              
              <Deployer
                communityGroupName={communityGroupName} 
                setCommunityGroupName={setCommunityGroupName} 
                mandatoryEntryFee={mandatoryEntryFee} 
                setMandatoryEntryFee={setMandatoryEntryFee} 
                deployContract={deployContract} 
                ConnectWallet={ConnectWallet} 
                stopContract={stopContract} 
                contractInfoSaved={contractInfoSaved} 
                isSavingContractInfo={isSavingContractInfo} 
              />

              : (activePage === "ERROR") ?

                <ErrorPage 
                errMessage={errMessage} 
                setActivePage={setActivePage} 
                errCode={errCode} 
                />
                
                :
                
                <div>
                  <h1> Oops!!, Unexpected Error.  </h1>
                </div>
      }
    </>
  );
}

export default App;



