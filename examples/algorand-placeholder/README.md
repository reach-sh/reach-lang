This example is a placeholder for demonstrating actual Algorand integration in Reach.

Currently:

* reach run will automatically run reachsh/algorand-devnet for you
* The index.mjs file exercises some simple functions in the Reach stdlib that interact with the devnet.
* CircleCI will run these examples by pulling reachsh/algorand-devnet from dockerhub

Near future:

* compile a nontrivial index.rsh file to Algorand; deploy the contract & run the backend via index.mjs
* get CircleCI to build reachsh/algorand-devnet from the Dockerfile and test this using that
