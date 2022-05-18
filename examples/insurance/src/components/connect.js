import React from 'react';
import loadingGif1 from '../images/ajax-loader.gif';
import errorIcon from '../images/error.png';

const Connect = ({connecting, setDeployerModeOn, deployerModeOn, loginErr, ConnectWallet, mnemonicRef, mnemonicStr, setMnemonicStr, LoginWithMnemonic}) => {
  return (
     <div className='h-screen flex bg-blue-100'>
      <div className='w-full max-w-md m-auto bg-white rounded-lg border border-primaryBorder shadow-default py-2 px-16 shadow'>
        <h1 className='text-4xl text-blue-700  text-primary mt-2 mb-2 text-center'> Insurance Dapp
          <span>
            <button onClick={setDeployerModeOn.bind(this, (!deployerModeOn))} className="pl-4 text-bold">
              [{deployerModeOn && "d"}]
            </button>
          </span>
        </h1>
        <hr />

        {(loginErr !== "") && <> <span className="text-red-400">{loginErr}</span> <span><img src={errorIcon} width="30px" alt="" /></span> <hr /> </>}
        <br />
        <button onClick={ConnectWallet}
          className={`w-full bg-green-500 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}
        >
          {connecting && <span> <img src={loadingGif1} width="12px" alt='' /> </span>}
          <span>{connecting ? "Please wait ..." : "Connect algo wallet"}</span>
        </button>
        <hr />
        <br />
        OR
        <h3 className='text-l font-medium text-primary mt-6 mb-6 text-center'>
          Enter Your mnemonic to Login
        </h3>

        <form >
          <div>
            <input ref={mnemonicRef}
              type="password"
              value={mnemonicStr}
              onChange={e => setMnemonicStr(e.target.value)}
              className={`w-full p-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
              id='mnemonic'
              placeholder='12-word phrase'
            />
          </div>

          <div>
            <label>Have no Algorand account ? <a target="_blank" href="https://perawallet.app/" className='text-blue-600' rel="noreferrer">Create a new</a> </label>
          </div>

          <div className='flex justify-center items-center mt-6'>
            <button onClick={LoginWithMnemonic}
              className={` bg-blue-500 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}
            >
              {connecting && <span> <img src={loadingGif1} width="12px" alt='' /> </span>}
              <span>{connecting ? "Wait ..." : "Login"}</span>
            </button>
          </div>
        </form>
      </div>
    </div>
  );
};
export default Connect;


