'reach 0.1 exe';

/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

const Asserter = participant({});
const Challenger = participant({});

const DELAY = 10; // in blocks

const K = 80;

function inside(Asserter, Challenger,
                lowerHash, upperHash, chainLength,
                callback) {
  Challenger.only(() => {
    const challengeHuh = declassify(is(bool, interact.assert(lowerHash, upperHash, chainLength))); } );
  Challenger.publish(challengeHuh)
    .timeout(DELAY, Asserter, () => {
      callback(true);
      commit();
      return; });

  if ( ! challengeHuh ) {
    callback(true);
    commit();
    return; }
  else {
    var [lower, upper, segments]
        = [lowerHash, upperHash, chainLength];
    invariant(balance() == 0);
    while (segments > 1) {
      const [ firstLength, restLength, lastSegment ] =
            ( segments < K ) ? [ 1, 1, segments - 1 ] :
            [ segments/K + segments%K, segments/K, K-1 ];
      assert(restLength <= firstLength);
      assert(firstLength < segments);
      commit();
      Asserter.only(() => {
        const newHashes = declassify(is(uint256[K], interact.split(lower, upper, K)));
        assume(newHashes[0] == lower);
        assume(newHashes[lastSegment] == upper); });
      Asserter.publish(newHashes)
        .timeout(DELAY, _, () => {
          callback(false);
          commit();
          return; });
      require(newHashes[0] == lower);
      require(newHashes[lastSegment] == upper);
      commit();
      Challenger.only(() => {
        const challengedSegment = declassify(is(uint256, interact.challengedSegment(newHashes, segments)));
        assume(challengedSegment < lastSegment); });
      Challenger.publish(challengedSegment)
        .timeout(DELAY, _, () => {
          callback(true);
          commit();
          return; });
      require(challengedSegment < lastSegment);

      [lower, upper, segments]
        = [ newHashes[challengedSegment],
            newHashes[challengedSegment+1],
            (challengedSegment == 0) ? firstLength : restLength ];
      continue; }

    commit();
    Asserter.only(() => {
      const value = declassify(is(uint256, interact.onestepproof(lower, upper)));
      assume(digest(lower, value) == upper) });
    Asserter.publish(value)
      .timeout(DELAY, _, () => {
        callback(false);
        commit();
        return; });
    require(digest(lower, value) == upper);
    callback(true);
    commit();
    return; } }

const Rollup = participant({
  _CallbackDest: address,
  _Asserter: address,
  _Challenger: address,
  _lowerHash: uint256,
  _upperHash: uint256,
  _chainLength: uint256
});

function main() {
  Rollup.only(() => {
    const CallbackDest = declassify(_CallbackDest);
    const Asserter = declassify(_Asserter);
    const Challenger = declassify(_Challenger);
    const lowerHash = declassify(_lowerHash);
    const upperHash = declassify(_upperHash);
    const chainLength = declassify(_chainLength); });
  Rollup.publish(CallbackDest, Asserter, Challenger,
                 lowerHash, upperHash, chainLength);
  commit();

  inside(Asserter, Challenger,
         lowerHash, upperHash, chainLength,
         ((answer) => {
           const winner = answer ? Asserter : Challenger;
           const loser = answer ? Challenger : Asserter;
           CallbackDest.call.resolveChallenge(winner, loser); } ));

  return true;
}
