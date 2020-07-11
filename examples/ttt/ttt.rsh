'reach 0.1 exe';

// Tic Tac Toe
const board_mt = array( false, false, false,
                        false, false, false,
                        false, false, false );

function ttt_initial(XisFirst) {
  return [ XisFirst, board_mt, board_mt ]; }

function ttt_turn_is_x(st) {
  const [ turn, xs, os ] = st;
  return turn; }

function marks_x(st) {
  const [ turn, xs, os ] = st;
  return xs; }

function marks_o(st) {
  const [ turn, xs, os ] = st;
  return os; }

function cell_both(st, i) {
  return marks_x(st)[i] || marks_o(st)[i]; }

function marks_all(st) {
  return array(
    cell_both(st, 0), cell_both(st, 1), cell_both(st, 2),
    cell_both(st, 3), cell_both(st, 4), cell_both(st, 5),
    cell_both(st, 6), cell_both(st, 7), cell_both(st, 8) ); }

function read_cell(b, c) {
  return b[c]; }

function cell(r, c) {
  return c + r * COLS; }

function seq(b, r, c, dr, dc) {
  return read_cell(b, cell(r, c))
    && read_cell(b, cell(r+dr, c+dc))
    && read_cell(b, cell(r+dr+dr, c+dc+dc)); }

function row(b, r) {
  return seq(b, r, 0, 0, 1); }

function col(b, c) {
  return seq(b, 0, c, 1, 0); }

function winning_p(b) {
  return row(b, 0) || row(b, 1) || row(b, 2)
    || col(b, 0) || col(b, 1) || col(b, 2)
    || seq(b, 0, 0, 1, 1)
    || seq(b, 0, 2, 1, -1); }

function complete_p(b) {
  return b[0] && b[1] && b[2]
    && b[3] && b[4] && b[5]
    && b[6] && b[7] && b[8]; }

function ttt_done(st) {
  return (winning_p(marks_x(st))
          || winning_p(marks_o(st))
          || complete_p(marks_all(st))); }

function validMove(st, m) {
  return ( true
           // Within range
           && 0 <= m
           && m < CELLS
           // spot is not taken
           && (! read_cell(marks_all(st), m)) ); }

function getValidMove(st) {
  const _m = is(uint256, interact.getMove(st));
  assume(validMove(st, _m));
  return declassify(_m); }

function applyMove(st, m) {
  const [ turn, xs, os ] = st;
  require(validMove(st, m));
  return [ ! turn,
           (turn ? array_set(xs, m, true) : xs),
           (turn ? os : array_set(os, m, true)) ]; }

function ttt_winner_is_x( st ) {
  return winning_p(marks_x(st)); }

function ttt_winner_is_o( st ) {
  return winning_p(marks_o(st)); }

// Protocol
const A = newParticipant();
const B = newParticipant();
const DELAY = 10; // in blocks

function main() {
  A.only(() => {
    const wagerAmount = declassify(is(uint256, interact.getWagerAmount()));
    const _coinFlipA = random();
    const commitA = declassify(digest(_coinFlipA)); });
  A.publish(wagerAmount, commitA)
    .pay(wagerAmount);
  commit();

  B.only(() => {
    interact.acceptWager(wagerAmount);
    const coinFlipB = declassify(random()); });
  B.publish(coinFlipB)
    .pay(wagerAmount)
    .timeout(DELAY, closeTo(A, "B never accepted"));
  commit();

  A.only(() => {
    const coinFlipA = declassify(_coinFlipA); });
  A.publish(coinFlipA)
    .timeout(DELAY, closeTo(B, "A never revealed coinflip"));

  require(commitA == digest(coinFlipA));
  const XisFirst = (( coinFlipA + coinFlipB ) % 2) == 0;

  var [ state ] = [ ttt_initial(XisFirst) ];
  invariant(balance() == (2 * wagerAmount));
  while ( ! ttt_done(state) ) {
    if ( ttt_turn_is_x( state ) ) {
      commit();

      A.only(() => {
        const moveA = getValidMove(state); });
      A.publish(moveA)
        .timeout(DELAY, closeTo(B, "A timed out move"));

      [ state ] = applyMove(state, moveA);
      continue; }
    else {
      commit();

      B.only(() => {
        const moveB = getValidMove(state); });
      B.publish(moveB)
        .timeout(DELAY, closeTo(A, "B timed out move"));

      [ state ] = applyMove(state, moveB);
      continue; } }

  const [ toA, toB ] =
        (ttt_winner_is_x( state ) ? [ 2, 0 ]
         : (ttt_winner_is_o( state ) ? [ 0, 2 ]
            : [ 1, 1 ]));
  transfer(toA * wagerAmount).to(A);
  transfer(toB * wagerAmount).to(B);
  commit();

  interact.showOutcome(state);
  return "Game is over"; }
