'reach 0.1';

// Tic Tac Toe
const ROWS = 3;
const COLS = 3;
const CELLS = ROWS * COLS;
const Board = Array(Bool, CELLS);
const State = Object({ xsTurn: Bool,
                       xs: Board,
                       os: Board });

const boardMt =
      Array.replicate(9, false);

const tttInitial = (XisFirst) =>
      ({ xsTurn: XisFirst,
         xs: boardMt,
         os: boardMt });

const cellBoth = (st, i) =>
      (st.xs[i] || st.os[i]);

const marksAll = (st) =>
      Array.iota(9).map(i => cellBoth(st, i));

const cell = (r, c) => c + r * COLS;

const op = (op, rhs) => (lhs) => op(lhs, rhs);

const seq = (b, r, c, dr, dc) =>
      (b[cell(r, c)] &&
       b[cell(r+dr, dc(c))] &&
       b[cell(r+dr+dr, dc(dc(c)))]);

const row = (b, r) => seq(b, r, 0, 0, op(add, 1));
const col = (b, c) => seq(b, 0, c, 1, op(add, 0));

const winningP = (b) =>
      (row(b, 0) || row(b, 1) || row(b, 2) ||
       col(b, 0) || col(b, 1) || col(b, 2) ||
       seq(b, 0, 0, 1, op(add, 1)) ||
       seq(b, 0, 2, 1, op(sub, 1)));

const completeP = (b) => b.and();

const tttDone = (st) =>
      (winningP(st.xs)
       || winningP(st.os)
       || completeP(marksAll(st)));

const legalMove = (m) => (0 <= m && m < CELLS);
const validMove = (st, m) => (! cellBoth(st, m));

function getValidMove(interact, st) {
  const _m = interact.getMove(st);
  assume(legalMove(_m));
  assume(validMove(st, _m));
  return declassify(_m); }

function applyMove(st, m) {
  require(legalMove(m));
  require(validMove(st, m));
  const turn = st.xsTurn;
  return { xsTurn: ! turn,
           xs: (turn ? st.xs.set(m, true) : st.xs),
           os: (turn ? st.os : st.os.set(m, true)) }; }

const tttWinnerIsX = ( st ) => winningP(st.xs);
const tttWinnerIsO = ( st ) => winningP(st.os);

// Protocol
const DELAY = 20; // in blocks

const Player =
      { ...hasRandom,
        getMove: Fun([State], UInt),
        endsWith: Fun([State], Null) };
const Alice =
      { ...Player,
        getWager: Fun([], UInt) };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt], Null) };

export const main =
  Reach.App(
    {},
    [Participant('A', Alice), Participant('B', Bob)],
    (A, B) => {
      A.only(() => {
        const wagerAmount = declassify(interact.getWager());
        const _coinFlipA = interact.random();
        const commitA = declassify(digest(_coinFlipA)); });
      A.publish(wagerAmount, commitA)
        .pay(wagerAmount);
      commit();

      B.only(() => {
        interact.acceptWager(wagerAmount);
        const coinFlipB = declassify(interact.random()); });
      B.publish(coinFlipB)
        .pay(wagerAmount);
      commit();

      A.only(() => {
        const coinFlipA = declassify(_coinFlipA); });
      A.publish(coinFlipA);

      require(commitA == digest(coinFlipA));
      const XisFirst = (((coinFlipA % 2) + (coinFlipB % 2)) % 2) == 0;

      var state = tttInitial(XisFirst);
      invariant(balance() == (2 * wagerAmount));
      while ( ! tttDone(state) ) {
        if ( state.xsTurn ) {
          commit();

          A.only(() => {
            const moveA = getValidMove(interact, state); });
          A.publish(moveA);

          state = applyMove(state, moveA);
          continue; }
        else {
          commit();

          B.only(() => {
            const moveB = getValidMove(interact, state); });
          B.publish(moveB);

          state = applyMove(state, moveB);
          continue; } }

      const [ toA, toB ] =
            (tttWinnerIsX( state ) ? [ 2, 0 ]
             : (tttWinnerIsO( state ) ? [ 0, 2 ]
                : [ 1, 1 ]));
      transfer(toA * wagerAmount).to(A);
      transfer(toB * wagerAmount).to(B);
      commit();

      each([A, B], () => {
        interact.endsWith(state); }); });
