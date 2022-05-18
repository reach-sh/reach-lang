import React from 'react';
import loadingGif1 from '../images/ajax-loader.gif';

const Deployer = ({communityGroupName, setCommunityGroupName, mandatoryEntryFee, setMandatoryEntryFee, deployContract, ConnectWallet, stopContract, contractInfoSaved, isSavingContractInfo }) => {
  //Insurer is the Deployer
  return (
     <div className='h-screen flex bg-blue-100'>
        <div className='w-full max-w-md m-auto bg-white rounded-lg border border-primaryBorder shadow-default py-2 px-16 shadow'>
          <h1 className='text-4xl text-blue-700  text-primary mt-2 mb-2 text-center'> Insurance Dapp </h1>
          <hr />

          <h1 className='text-xl font-medium text-primary mt-6 mb-6 text-center'>
            Deploy the contract
          </h1>

          <form >
            <div>
              <label htmlFor='fullname'> Insurer name </label>
              <input
                type="text"
                value={communityGroupName}
                onChange={e => setCommunityGroupName(e.target.value)}
                className={`w-full py px-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
              />
            </div>

            <div>
              <label htmlFor='phone'> Mandatory entry fee </label>
              <input
                type="number"
                value={mandatoryEntryFee}
                onChange={e => setMandatoryEntryFee(e.target.value)}
                className={`w-full py px-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
              />
            </div>

            <div className='flex justify-center items-center mt-6'>
              <button onClick={deployContract}
                className={`bg-blue-500 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark ${(isSavingContractInfo || contractInfoSaved) ? "opacity-50 cursor-not-allowed" : ""}`}
              >
                {contractInfoSaved ? "Deployed" : isSavingContractInfo ? "Deploying ..." : "Deploy contract now"}
              </button>
              {
                isSavingContractInfo && <span>
                  <img src={loadingGif1} width="40px" alt='' />
                </span>
              }
              {contractInfoSaved && 
                <button onClick={stopContract}
                  className={`bg-blue-500 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}
                > Stop contract </button>
              }
              <hr/>
              <button onClick={ConnectWallet}
                  className={`bg-gray-400 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}
                > Exit </button>
            </div>
          </form>
        </div>
    </div >
  );
};
export default Deployer;
