'reach 0.1';
'use strict';

const REWARD_FOR_PROVER = 1000000000000000000//send by VERIFIER

//NOTES:
// TODO: This smart contract is empower to validate if the positions if user are correct
// There is a smart contract for every different position
// TODO: The Smart Contract will expire after a specific amount of time
// TODO: Add the geofence attribute to the smart contract (radius, etc etc ....). In this way
//       we can design more checks such as: the position received must be inside the geofence area.
// TODO: The smart contract will know the verifier (?) ----> still to be decided.
//       Maybe using an unique password (for more verifiers) for memory reason
// TODO: check that the proofs inserted unique and not already present


export const main = Reach.App(() => {
  const Creator = Participant('Creator', {
    ...hasConsoleLogger,
    position: Bytes(128),
    decentralized_identifier: UInt,
    proof_reveived: Bytes(128),
    reportPosition: Fun([UInt, Maybe(Bytes(128))], Null),

  });



  const attacherAPI = API('attacherAPI', {
    insert_position: Fun([Bytes(128), UInt], UInt), //PositionAndProof - DID - ReturnField
  });

  const verifierAPI = API('verifierAPI', {
    verify: Fun([UInt, Address], Bool),
    insert_money: Fun([UInt], UInt),
  });

  setOptions({ untrustworthyMaps: true });
  init();

  Creator.publish() //we need that to use the MAP below
  const easy_map = new Map(UInt, Bytes(128));

  commit();
  Creator.only(() => {
    const proof_and_position = declassify(interact.position);
    const decentralized_identifier_creator = declassify(interact.decentralized_identifier);
  });


  Creator.publish(proof_and_position, decentralized_identifier_creator); //TODO: add the proof_received

  easy_map[decentralized_identifier_creator] = proof_and_position; //setting the first value of the map with Creator values

  commit();
  Creator.publish();
  //setting the view
  Creator.only(() => interact.reportPosition(decentralized_identifier_creator, easy_map[decentralized_identifier_creator]));

  // ************ INSERT POSITION API **************
  // the API terminated whe it reaches 3 users
  //the attacher can insert their positions
  const counter =
    parallelReduce(3)
      .invariant(balance() == balance()) // invariant: the condition inside must be true for the all time that the while goes on
      //.define(() => {views.retrieve_results.set(did_user);}) // define: the code inside is executed when a function in the while is called (ex. the api call)
      .while(counter >= 0)
      .api(attacherAPI.insert_position, // the name of the api that is called
        (pos, did, y) => { // the code to execute and the returning variable of the api (y)
          y(did);
          Creator.only(() => interact.reportPosition(counter, easy_map[did]));
          //TODO: notify the attacher (not the creator) when the key is already used
          if (easy_map[did] != Null) { //TODO: FIX THIS CHECK. CHECK if map contain THE ID INSERTED --------------> IMPORTANT
            const stopLoop = 0
            return stopLoop; //TODO: THIS HAS TO RETURN TRUE when the API return bool. Now return an UInt just for testing
          }
          /**
           * The line below manages the case when the key is already
           * assigned to a specific value
           * */
          easy_map[did] = fromSome(easy_map[did], pos);

          Creator.only(() => interact.reportPosition(counter, easy_map[did]));

          //TODO: ONLY for TESTING: terminate the parallel reduce


          return counter - 1;
        }
      )
  // TIMEOUT WORKS ONLY ON TESTNET
  // .timeout(relativeTime(deadline), () => { // timeout: function that executes code every amount of time decided by the first parameter
  //   Creator.interact.log("The campaign has finished") // log on the Creator cli to inform the end of the campaign
  //   Anybody.publish(); // publish needed to finish the parallel reduce
  //   return [total_balance,false]; // set keepGoing to false to finish the campaign
  // });


  const keepGoing2 =
    parallelReduce(true)
      .invariant(balance() == balance())
      .while(keepGoing2)
      .api(verifierAPI.insert_money,
        (money) => { // the assume that have to be true to continue the execution of the API
          assume(money > 0);
        },
        (money) => money, // the payment that the users have to do when call the api
        (money, y) => {
          y(money);


          return true;
        }
      )
      .api(verifierAPI.verify,
        (did, walletAddress, ret) => {
          // transfer some money to the Prover (attacher)
          if (balance() >= REWARD_FOR_PROVER) {
            transfer(REWARD_FOR_PROVER).to(walletAddress);
            ret(true);

          }
          ret(false);
          delete easy_map[did]; //vector[0] is the did

          return false; //TODO: THIS HAS TO BE TRUEE, false only for testing
        }
      )


  // TODO: the first received position has to be stored in a data structure, will be compared to the subsquent received positions

  //for TESTING
  transfer(balance()).to(Creator);

  commit();


  exit();
});
