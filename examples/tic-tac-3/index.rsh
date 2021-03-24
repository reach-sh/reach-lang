'reach 0.1';

const Fees = Array(UInt, 9)
const CELLS = 9;
const Board = Array(Bool, CELLS);
const State = Object({ xsTurn: Bool,
                       xs: Board,
                       os: Board });

const boardMt = Array.replicate(9, false);

// Create the array for the fees, which is aligned with the state array.
const fee_matrix = array(UInt, [3, 2, 3, 2, 4, 2, 3, 2, 3]);

const fee_realized = (st, fee) => Array.iota(9).map((i) => st[i] ? fee[i]:0);

const sum_fee = (st, fee) => (fee_realized(st, fee)).sum();

const tttInitial = (XisFirst) =>
      ({ xsTurn: XisFirst,
         xs: boardMt,
         os: boardMt });

const occupied_cell = (st, i) => (st.xs[i] || st.os[i]);

const occupied_matrix = (st) => Array.iota(9).map(i => occupied_cell(st, i));

const wins = (b) =>
    (
    // horizontal
    ((b[0] && b[1] && b[2]) ? 1:0) 
    +((b[3] && b[4] && b[5]) ? 1:0)
    +((b[6] && b[7] && b[8]) ? 1:0)
    // vertical
    +((b[0] && b[3] && b[6]) ? 1:0)
    +((b[1] && b[4] && b[7]) ? 1:0)
    +((b[2] && b[5] && b[8]) ? 1:0)
    // diagonal
    +((b[0] && b[4] && b[8]) ? 1:0) 
    +((b[2] && b[4] && b[6]) ? 1:0)
    )

// true if there is a winner
const winner = (b) => wins(b) > 0

const ttt_winner_is_X = ( st ) => winner(st.xs);
const ttt_winner_is_O = ( st ) => winner(st.os);

// Check if the game was a draw, aka the state is full
const draw = (board) => board.and();

const game_over = (st) =>
      (winner(st.xs)
       || winner(st.os)
       || draw(occupied_matrix(st)));

const is_legal = (m) => (0 <= m && m < CELLS);
const is_valid = (st, m) => (! occupied_cell(st, m));

function getValidMove(interact, st) {
  const _m = interact.getMove(st, fee_matrix);
  assume(is_legal(_m));
  assume(is_valid(st, _m));
  return declassify(_m); }

function applyMove(st, m) {
  require(is_legal(m));
  require(is_valid(st, m));
  const turn = st.xsTurn;
  return { xsTurn: ! turn,
           xs: (turn ? st.xs.set(m, true) : st.xs),
           os: (turn ? st.os : st.os.set(m, true)) }; }



const Player =
      { ...hasRandom,
        getMove: Fun([State, Board], UInt),
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
      while ( ! game_over(state) ) {
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

      const x_wager = (sum_fee(state.xs, fee_matrix))*wagerAmount/16
      const o_wager = (sum_fee(state.os, fee_matrix))*wagerAmount/16
      const x_unspent = wagerAmount - x_wager
      const o_unspent = wagerAmount - o_wager
      const pot = x_wager + o_wager
      // bool logical winner
      const x_win = winner_is_x(state) 
      const o_win = winner_is_o(state)
      const tie = !( x_win || o_win)        
      // At the end of the game, divy funds to the correct party
      const [toA, toB] = (
          x_win ? [pot+x_unspent, o_unspent]
          : o_win ? [x_unspent, pot+o_unspent]
          : [o_wager+x_unspent, x_wager+o_unspent]
      );
      transfer(toA).to(A);
      transfer(toB).to(B);
      commit();

      each([A, B], () => {
        interact.endsWith(state); }); });
