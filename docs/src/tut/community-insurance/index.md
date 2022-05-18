# {#tut} Community insurance

This tutorial walks through the creation of a Reach decentralized application with React (v16.8 - Hooks) as the frontend.
It demonstrates how you can use reach together with react to build a real world community insurance application.
To follow along, you are expected to have already gone through [the most basic tutorial](##tut) and you have already
learnt how to develop and test with a command-line frontend. 
We highly encourage you to read about [the reach architecture](https://docs.reach.sh/rsh/#ref-programs) as well, 
as it's an excellent resource to help you understand the different modes/states of any Reach program.

## Assumptions
This tutorial assumes that you already
 * have [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
 * have node & npm installed on your machine. If not, first head over to the [official website](https://nodejs.org/en/download/) 
    and follow the installation steps.
 * have working knowledge of react hooks. You can checkout [the basics](https://reactjs.org/docs/hooks-overview.html#:~:text=Hooks%20are%20functions%20that%20let,if%20you'd%20like.)) in case this is new to you.
 * installed Reach successfully by following the [installation process](##tut) from the basic tutorial. 

Since this is a real world application, we want to style it professionally using [tailwind css](https://tailwindcss.com/). You don't have to be familiar with this framework to follow along though. 
We'll provide a step by step configuration process to enable this styling framework.

In case you want to first have some background knowledge on the insurance business, checkout [this pdf doc](https://ira.go.ug/cp/uploads/English%20Handbook%20final.pdf).

## Preparation
* Complete the most [basic tutorial](##tut), finish the installation of Reach and all related configuration such as Docker.
* Initialize a react app (using [create-react-app](https://tailwindcss.com/docs/guides/create-react-app)) and configure tailwind css.

## Initialize the application
Go to the directory where you wish to create your project and open a new terminal from there.

Run these commands to initialise the project
```cmd
$ npx create-react-app community-insurance-dapp
$ cd community-insurance-dapp
```

To Install Tailwind CSS,
```cmd
$ npm install -D tailwindcss postcss autoprefixer
$ npx tailwindcss init -p
```

To Configure Tailwind CSS, open the file called `tailwind.config.js` at the root and paste in the following code.
```
load: /examples/insurance/tailwind.config.js
```
Open your `src/index.css` file and paste the code below, overwriting everything inside.
```
load: /examples/insurance/src/index.css
```

At the root of your application code, there a `package.json` file which contains all the configuration for react 
and it is responsible for keeping track of all the dependances required to run this application. 
Open it and paste the code bellow, overwriting everything inside.

```
load: /examples/insurance/package.json
```

Now run the command bellow and wait to for npm to finish installing all the React dependances
```cmd
$ npm install
```

Preparation is done. Now lets start writing our Reach application code.

## Create the first version of your Reach program for the community insurance dapp
TODO:





Thanks for spending your afternoon with us!
