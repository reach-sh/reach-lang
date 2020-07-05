#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Reach Tutorials}

This document contains a series of guiding tutorials to introduce the key concepts of the Reach language and @DApp design. In contrast, the @secref{ref} contains an exhaustive discussion of each of the parts of the language and standard library.

@local-table-of-contents[#:style 'immediate-only]

XXX

@reach{
'reach 0.1 exe';

// Game
function applyMove(AsTurn, heap1, heap2, choose1, amount) {
  require(amount <= (choose1 ? heap1 : heap2))
  if ( choose1 ) {
    return [ !AsTurn, heap1 - amount, heap2 ];
  } else {
    return [ !AsTurn, heap1, heap2 - amount ]; } }

// Protocol
const A = participant({});
const B = participant({});
const DELAY = 10; // in blocks

function main() {
  A.only(() => {
    const wagerAmount = declassify(is(uint256, interact.getWagerAmount()));
    const initialHeap = declassify(is(uint256, interact.getInitialHeap()));
    const _coinFlipA = random();
    const commitA = declassify(digest(_coinFlipA));});
  A.publish(wagerAmount, initialHeap, commitA)
    .pay(wagerAmount)
    .timeout(DELAY, () => {
      B.publish();
      commit();
      return "A never started"; });
  commit(); }
}

