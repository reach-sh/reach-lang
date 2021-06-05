/* 17 U.S.C. §§ 101-800. */
/* Copyright Archie Chaudhury and Brian Haney 2021 */
/* MIT License */
/* Permission is hereby granted, free of charge, */
/* to any person obtaining a copy of this software and associated documentation files (the "Software"), */
/* to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, */
/* and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions: */

/* The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. */

/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, */
/* INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. */
/* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, */ 
/* ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */
 /*----------------------------------------------------------------------------------------------------------*/

'reach 0.1';

 /* Declares the possible options for the vote and sets an array of length 2 for it. */
const [ isVote, NO, YES] = makeEnum(2);

 /* Declares the possibilities for the final result and sets an array of length 3 */
const [ finalResult, FAIL, DRAW, SUCCESS ] = makeEnum(3);

/* Declares the formula for the consensus. */
/* Essentially states that if NO is 0 and YES is 1, then adding these values from the two different voters. */
/* The final result should be from the finalResult array.  */
const consensus = (voteA, voteB) =>
      voteA + voteB;

/* Declares Assertions for what the final value should be and then checks these values. */
/* Line 9 asserts a draw definition. */
/* Line 10 asserts a success definition. */
/* Line 11 asserts a fail definition. */
assert(consensus(NO, YES) ==  DRAW);
assert(consensus(YES, YES) == SUCCESS);
assert(consensus(NO, NO) == FAIL);

/* Lines 13 - 17 declare the Voter class.*/
const Voter =
      { ...hasRandom,
        getVote: Fun([], UInt),
        seeResult: Fun([UInt], Null),
        informTimeout: Fun([], Null) };

/* Lines 18 - 23 declares two Voter objects, Alice and Bob.*/
/* Lines 18 - 20 declares Alice using the voter class an interger stake.*/
/* Lines 21 - 23 declares Bob using the voter class an function with integer and null arguments.*/
const Alice =
      { ...Voter,
        stake: UInt };
const Bob =
      { ...Voter,
        acceptStake: Fun([UInt], Null) };

/* Line 25 defines what particpants can do in regards to timeouts. */
const DEADLINE = 30;

/* Liens 27 - 29 defines Reach application. */
export const main =
  Reach.App(
    {},

    /* Line 30 defines a participant list. */
    [Participant('Alice', Alice), Participant('Bob', Bob)],
    (A, B) => {
      const informTimeout = () => {
        each([A, B], () => {
          interact.informTimeout(); }); };
      
      /* Lines 35 - 40 allow Alice, the validator, to publish the stake and ticket price, and then ensures that B accepts it. */
      A.only(() => {
        const stake = declassify(interact.stake); });
      A.publish(stake)
        .pay(stake);
      commit();

      /* Lines 35 - 40 allow Bob to interact and stake. */
      B.only(() => {
        interact.acceptStake(stake); });
      B.pay(stake)
        .timeout(DEADLINE, () => closeTo(A, informTimeout));

      /* Lines 41-42 defines a draw result and associated balance. */
      var result = DRAW;
      invariant(balance() == 2 * stake);

      /* Lines 43 - 67 defines a draw function using a while loop. */
      while ( result == DRAW ) {
        commit();

        /* Lines 48 - 51 defines a function for A which calls the getVote function. */
        A.only(() => {
          const _voteA = interact.getVote();
          const [_decisionA, _saltA] = makeCommitment(interact, _voteA);
          const decisionA = declassify(_decisionA); });
        A.publish(decisionA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        commit();
        unknowable(B, A(_voteA, _saltA));

        /* Lines 56 - 60 defines a function for B which calls the getVote function. */
        B.only(() => {
          const voteB = declassify(interact.getVote()); });
        B.publish(voteB)
          .timeout(DEADLINE, () => closeTo(A, informTimeout));
        commit();

        /* Lines 61 - 64 defines a function for A which includes a timeout method. */
        A.only(() => {
          const [saltA, voteA] = declassify([_saltA, _voteA]); });
        A.publish(saltA, voteA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        checkCommitment(decisionA, saltA, voteA);

         /* Lines 66 - 67 Set a loop to ensure the results are properly compiled, allowing an organization to reach consensus */
        result = consensus(voteA, voteB);
        continue; }

      /* Lines 68 - 69 define a success transfer. */
      transfer(2 * stake).to(result == SUCCESS ? A : B);
      commit();

      /* Lines 70 - 71 allows the voters to see the result*/
      each([A, B], () => {
        interact.seeResult(result); });
        
      exit(); });