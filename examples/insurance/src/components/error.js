import React from 'react';

const ErrorPage = ({errMessage, setActivePage, errCode}) => {
  return (
    <div>
      <h1> Error ! </h1>
      <p> {errMessage} </p>
      <br />
      <hr />
      {
        errCode === "GOTO_LOGIN" ?
          <button onClick={setActivePage.bind(this, "LOGIN")}>Go back to login </button>
          : errCode === "GOTO_SIGNUP" ?
            <button onClick={setActivePage.bind(this, "SIGNUP")}>Go back to login </button>
            :
            <button onClick={setActivePage.bind(this, "LOGIN")}>Go back to login </button>
      }
    </div>
  );
};

export default ErrorPage;
