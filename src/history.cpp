# include "history.h"

namespace Luna {

// this only works prior to us making the move
int History::get_history(Position *p, Move m, HistoryMove previous, HistoryMove followup) const {
  // we use capture history if the move is a capture
  bool isCapture = p->is_capture(m);
  if (isCapture)
    return get_capture_hist(p->piece_moved(m), to_sq(m), piece_type(p->pc_sq(to_sq(m))));
  // we use all our other histories for quiet moves
  // quiet moves use more heuristics since they are more difficult to evaluate
  else {
    // we also calculate basic threats to help when scoring a quiet move
    U64 threats, threatsPawn, threatsMinor, threatsRook;
    Color us = p->get_side();
    // calculate threats
    threatsPawn = p->attacks_by(!us, PAWN);
    threatsMinor = p->attacks_by(!us, KNIGHT) | p->attacks_by(!us, BISHOP) | threatsPawn;
    threatsRook = p->attacks_by(!us, ROOK) | threatsMinor;
    threats = (p->pc_bb(QUEEN, us) & threatsRook)
            | (p->pc_bb(ROOK, us) & threatsMinor)
            | ((p->pc_bb(KNIGHT, us) | p->pc_bb(BISHOP, us)) & threatsPawn);

    int butterfly = get_butterfly(us, m);
    int counter = (get_counter_hist(previous.pc, to_sq(previous.m)) == m) ? 5000 : 0;
    int follow = (get_followup_hist(followup.pc, to_sq(followup.m)) == m) ? 3000 : 0;
    int thr = (threats & square_bb(from_sq(m)) ?
              (piece_type(p->piece_moved(m)) == QUEEN && !(to_sq(m) & threatsRook) ? 50000
             : piece_type(p->piece_moved(m)) == ROOK && !(to_sq(m) & threatsMinor) ? 25000
             : !(to_sq(m) & threatsPawn) ? 15000 : 0) : 0);
    return ((2 * butterfly + 2 * counter + 2 * follow) / 3) + thr;
  }
}

void History::clear() {
  memset(killers, 0, sizeof(killers));
  memset(butterflyHistory, 0, sizeof(butterflyHistory));
  memset(captureHistory, 0, sizeof(captureHistory));
  memset(counterHistory, 0, sizeof(counterHistory));
  memset(followupHistory, 0, sizeof(followupHistory));
  memset(evalHistory, 0, sizeof(evalHistory));
}

void History::set_killer(Color side, Move m, int ply) {
  if (killers[side][ply][0] != m) {
    killers[side][ply][1] = killers[side][ply][0];
    killers[side][ply][0] = m;
  }
}

bool History::is_killer(Color side, Move m, int ply) {
  return (m == killers[side][ply][0] || m == killers[side][ply][1]);
}

void History::set_eval_hist(Color side, int eval, int ply) {
  this->evalHistory[side][ply] = eval;
}

void History::set_capture_hist(Position *p, Move m, int val) {
  Square from = from_sq(m);
  Square to = to_sq(m);
  this->captureHistory[p->pc_sq(from)][to][piece_type(p->pc_sq(to))] = val;
}

void History::set_max_improvement(Square from, Square to, int value) {
  this->maxImprovement[from][to] = value;
}

void History::reset_killers(Color side, int ply) {
  this->killers[side][ply + 2][0] = 0;
  this->killers[side][ply + 2][1] = 0;
}

Move History::get_killer(Color side, int ply, int id) const {
  return killers[side][ply][id];
}

int History::get_butterfly(Color side, Move m) const {
  return butterflyHistory[side][m & 0xFFF];
}

int History::get_capture_hist(Piece pc, Square to, PieceType pt) const {
  return captureHistory[pc][to][pt];
}

Move History::get_counter_hist(Piece pc, Square to) const {
  return counterHistory[pc][to];
}

Move History::get_followup_hist(Piece pc, Square to) const {
  return followupHistory[pc][to];
}

int History::get_eval_hist(Color side, int ply) const {
  return evalHistory[side][ply];
}

int64_t History::get_spent_effort(Square s1, Square s2) const {
  return spentEffort[s1][s2];
}

int History::get_max_improvement(Square from, Square to) const {
  return maxImprovement[from][to];
}

bool History::is_improving(Color side, int eval, int ply) const {
  if (ply >= 2) return eval > get_eval_hist(side, ply - 2);
  return true;
}

void History::update_butterfly(Color side, Move m, int val) {
  butterflyHistory[side][sq_combo_idx(m)] += val - butterflyHistory[side][sq_combo_idx(m)] * abs(val) / 14000;
}

void History::update_captures(Piece moved, Square to, PieceType cap, int val) {
  captureHistory[moved][to][cap] += val - captureHistory[moved][to][cap] * abs(val) / 10000;
}

}
