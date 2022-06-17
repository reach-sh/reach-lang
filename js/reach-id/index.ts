import fetch from 'node-fetch';


// Types

type JWT = string;

type ID = string;

type Authorization = JWT;

type CreateAccount =
  { ca_firstName   : string
  , ca_lastName    : string
  , ca_email       : string
  , ca_password    : string
  , ca_phoneNumber : string
  , ca_tfaSms?     : string
  , ca_tfaEmail?   : string
  , ca_tfaTotp?    : boolean }

type ConfirmAccount =
  { id : string
  }

type GetAccountInfo =
  {

  }

type Login =
  { lf_username : string
  , lf_password : string
  }

type Logout =
  {
  }

type SendVerificationCode =
  { id : string
  }

type UpdateAccount =
  { ua_firstName   : string
  , ua_lastName    : string
  , ua_email       : string
  , ua_phoneNumber : string
  , ua_tfaSms?     : string
  , ua_tfaEmail?   : string
  , ua_tfaTotp?    : boolean }

type VerifyLogin =
  { vl_id?     : string
  , vl_verCode : string }

type User =
  { userId      : string
  , firstName   : string
  , lastName    : string
  , email       : string
  , password    : string
  , phoneNumber : string
  , tfaSms?     : string
  , tfaEmail?   : string
  , tfaTotp?    : boolean }

// Helpers

const port = 8080;
const url = `http://localhost:${port}`;

type QueryParam = [string, string];

const mkEndpoint = (endpoint: string, queryParams: QueryParam[] = []) => {
  const p = queryParams
              .map(([f, v]: QueryParam) => `${f}=${v}`)
              .join('&');
  const ps = (queryParams.length > 0) ? `?${p}` : ``;
  return `${url}/${endpoint}${ps}`;
}

const doFetch = async (method: string, url: string, msg: Object) => {
  const result = await fetch(url, ({
    method,
    // @ts-ignore
    credentials: 'same-origin',
    headers: {
      'Accept': 'application/json',
      'Content-type': 'application/json'
    },
    body: JSON.stringify(msg)
  }));
  return await result.json();
}

const get  = (url: string, msg: Object) => doFetch(`GET`, url, msg);
const post = (url: string, msg: Object) => doFetch(`POST`, url, msg);

// Methods

type Env = {
  jwt: JWT | undefined,
  uid: ID | undefined,
}

const env: Env = {
  jwt: undefined,
  uid: undefined
};

const login = async (info: Login): Promise<User> => {
  const url = mkEndpoint(`login`);
  const msg = info;
  const user = await post(url, msg) as User;
  env.uid = user.userId;
  return user;
}

const verifyLogin = async (info: VerifyLogin): Promise<void> => {
  // If not specified, use userId from state
  if (info.vl_id == undefined) {
    info.vl_id = env.uid;
  }

}
