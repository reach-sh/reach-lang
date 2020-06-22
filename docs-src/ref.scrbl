#lang scribble/manual
@(require "lib.rkt")

@title[#:version "" #:tag "ref"]{Reach Reference}

This document contains an exhaustive discussion of each of the parts
of the Reach @DApp language and its standard library. In contrast,
@secref{tut} contains a series of guiding tutorials to introduce the
key concepts of the language and @DApp design.

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
    .timeout(DELAY, _, () => {
      commit();
      return "A never started"; });
  commit(); }
}
