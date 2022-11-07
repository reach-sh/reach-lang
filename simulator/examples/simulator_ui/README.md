# Getting Started with Create React App

This project was bootstrapped with [Create React App](https://github.com/facebook/create-react-app).
## Learn More

You can learn more in the [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started).

# Local development Environment

You can choose between running the standard dev environment, or using the storybook environment.

Standard Environment Command: `npm run start`

Storybook Environment Commmand: `npm run storybook`

You'll also need to proxy your requests to the server in order to avoid CORS errors.  

Install this globally:
https://www.npmjs.com/package/local-cors-proxy

Open a terminal window and run `lcp --proxyUrl http://localhost:3001`

Open node_modules/@reach-sh/simulator-client/client.mjs and edit line 6 to match the address you get back from the lcp. (ie: `http://localhost:8010/proxy`)

# Build

To build run `npm run build`



