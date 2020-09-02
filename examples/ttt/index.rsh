'reach 0.1';

// Tic Tac Toe
const ROWS = 3;
const COLS = 3;
const CELLS = ROWS * COLS;
const Board = Array(Bool, CELLS);
const State = Object({ xs_turn: Bool,
                       xs: Board,
                       os: Board });

const board_mt =
      array( Bool,
             [ false, false, false,
               false, false, false,
               false, false, false ] );

const ttt_initial = (XisFirst) =>
      ({ xs_turn: XisFirst,
         xs: board_mt,
         os: board_mt });

const cell_both = (st, i) =>
      (st.xs[i] || st.os[i]);

const marks_all = (st) =>
      array( Bool,
             [ cell_both(st, 0), cell_both(st, 1), cell_both(st, 2),
               cell_both(st, 3), cell_both(st, 4), cell_both(st, 5),
               cell_both(st, 6), cell_both(st, 7), cell_both(st, 8) ] );

const cell = (r, c) => c + r * COLS;

const seq = (b, r, c, dr, dc) =>
      (b[cell(r, c)] &&
       b[cell(r+dr, c+dc)] &&
       b[cell(r+dr+dr, c+dc+dc)]);

const row = (b, r) => seq(b, r, 0, 0, 1);
const col = (b, c) => seq(b, 0, c, 1, 0);

const winning_p = (b) =>
      (row(b, 0) || row(b, 1) || row(b, 2) ||
       col(b, 0) || col(b, 1) || col(b, 2) ||
       seq(b, 0, 0, 1, 1) ||
       seq(b, 0, 2, 1, -1));

const complete_p = (b) =>
      (b[0] && b[1] && b[2] &&
       b[3] && b[4] && b[5] &&
       b[6] && b[7] && b[8]);

const ttt_done = (st) =>
      (winning_p(st.xs)
       || winning_p(st.os)
       || complete_p(marks_all(st)));

const legalMove = (m) => (0 <= m && m < CELLS);
const validMove = (st, m) => (! marks_all(st)[m]);

function getValidMove(interact, st) {
  const _m = interact.getMove(st);
  assume(legalMove(_m));
  assume(validMove(st, _m));
  return declassify(_m); }

function applyMove(st, m) {
  require(legalMove(m));
  require(validMove(st, m));
  const turn = st.xs_turn;
  return { xs_turn: ! turn,
           xs: (turn ? st.xs.set(m, true) : st.xs),
           os: (turn ? st.os : st.os.set(m, true)) }; }

const ttt_winner_is_x = ( st ) => winning_p(st.xs);
const ttt_winner_is_o = ( st ) => winning_p(st.os);

// Protocol
const DELAY = 10; // in blocks

const Player =
      { ...hasRandom,
        getMove: Fun([State], UInt256),
        endsWith: Fun([State], Null) };
const Alice =
      { ...Player,
        getWager: Fun([], UInt256) };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt256], Null) };

export const main =
  Reach.App(
    {},
    [['A', Alice], ['B', Bob]],
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
      const XisFirst = (( coinFlipA + coinFlipB ) % 2) == 0;

      var state = ttt_initial(XisFirst);
      invariant(balance() == (2 * wagerAmount));
      while ( ! ttt_done(state) ) {
        if ( state.xs_turn ) {
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
            (ttt_winner_is_x( state ) ? [ 2, 0 ]
             : (ttt_winner_is_o( state ) ? [ 0, 2 ]
                : [ 1, 1 ]));
      transfer(toA * wagerAmount).to(A);
      transfer(toB * wagerAmount).to(B);
      commit();

      each([A, B], () => {
        interact.endsWith(state); }); });
