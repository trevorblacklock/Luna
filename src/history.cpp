# include "history.h"

namespace Luna {

int History::get_history(Position *p, Move m) const {
  assert(m);
  assert(check_move(m));
  int     val;
  U64     threats, threatByPawn, threatByMinor, threatByRook;
  int     ply  = p->get_ply();
  Color   us   = p->get_side();
  Square  from = from_sq(m);
  Square  to   = to_sq(m);
  Piece   pc   = p->pc_sq(from);
  Piece   cap  = p->pc_sq(to);

  if (p->is_capture(m)) {
    // retrieve capture histories
    val = (10 * PieceValue[MG][cap] + get_capture_hist(pc, to, piece_type(cap))) / 15;
  }
  else {

    PieceType pt = piece_type(pc);
    U64 t = square_bb(to);

    // initialize threats
    threatByPawn  = p->attacks_by(!us, PAWN);
    threatByMinor = p->attacks_by(!us, KNIGHT) | p->attacks_by(!us, BISHOP) | threatByPawn;
    threatByRook  = p->attacks_by(!us, ROOK) | threatByMinor;
    threats       = (p->pc_bb(QUEEN, us) & threatByRook)
                  | (p->pc_bb(ROOK, us) & threatByMinor)
                  | ((p->pc_bb(KNIGHT, us) | p->pc_bb(BISHOP, us)) & threatByPawn);

    // main histories
    val = 2 * get_butterfly(us, m);
    val += 2 * get_continuation_hist(pc, to, ply);
    val += get_continuation_hist(pc, to, ply - 1);
    val += get_continuation_hist(pc, to, ply - 2) / 4;
    val += get_continuation_hist(pc, to, ply - 3);
    val += get_continuation_hist(pc, to, ply - 5);

    // bonus for landing on a check square
    val += bool(p->checker_sq(pt) & square_bb(to)) * 15000;

    // bonus for escaping capture
    val += threats & square_bb(from) ? (pt == QUEEN && !(t & threatByRook) ? 50000
                                      : pt == ROOK && !(t & threatByMinor) ? 25000
                                      : !(t & threatByPawn)                ? 15000
                                      : 0) : 0;

    // if piece is moving into a threat
    val -= !(threats & square_bb(from)) ? (pt == QUEEN ? bool(t & threatByRook) * 50000
                                                       + bool(t & threatByMinor) * 10000
                                                       + bool(t & threatByPawn) * 20000
                                        : pt == ROOK ? bool(t & threatByMinor) * 25000
                                                     + bool(t & threatByPawn) * 10000
                                        : pt != PAWN ? bool(t & threatByPawn) * 15000
                                        : 0) : 0;
  }

  return val;
}

void History::clear() {
  memset(killers, 0, sizeof(killers));
  memset(butterflyHistory, 0, sizeof(butterflyHistory));
  memset(captureHistory, 0, sizeof(captureHistory));
  memset(evalHistory, 0, sizeof(evalHistory));
  memset(continuationHistory, 0, sizeof(continuationHistory));
  memset(maxImprovement, 0, sizeof(maxImprovement));
  bestMove = MOVE_NONE;
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

void History::set_continuation_hist(Piece pc, Square sq, int ply, int val) {
  this->continuationHistory[ply + 7][pc][sq] = val;
}

void History::set_capture_hist(Position *p, Move m, int val) {
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = p->pc_sq(from);
  PieceType pt = piece_type(p->pc_sq(to));
  this->captureHistory[pc][to][pt] = val;
}

void History::set_max_improvement(Square from, Square to, int value) {
  this->maxImprovement[from][to] = value;
}

void History::reset_killers(Color side, int ply) {
  this->killers[side][ply + 2][0] = 0;
  this->killers[side][ply + 2][1] = 0;
}

Move History::get_killer(Color side, int ply, int id) const {
  assert(ply >= 0 && ply < MAX_INTERNAL_PLY + 2);
  return this->killers[side][ply][id];
}

int History::get_butterfly(Color side, Move m) const {
  return this->butterflyHistory[side][sq_combo_idx(m)];
}

int History::get_capture_hist(Piece pc, Square to, PieceType pt) const {
  return this->captureHistory[pc][to][pt];
}

int History::get_continuation_hist(Piece pc, Square sq, int ply) const {
  return this->continuationHistory[ply + 7][pc][sq];
}

int History::get_eval_hist(Color side, int ply) const {
  return this->evalHistory[side][ply];
}

int64_t History::get_spent_effort(Square s1, Square s2) const {
  return this->spentEffort[s1][s2];
}

int History::get_max_improvement(Square from, Square to) const {
  return this->maxImprovement[from][to];
}

bool History::is_improving(Color side, int eval, int ply) const {
  if (ply >= 4) return eval > get_eval_hist(side, ply - 2)
                    && eval > get_eval_hist(side, ply - 4);
  if (ply >= 2) return eval > get_eval_hist(side, ply - 2);
  return true;
}

void History::update_butterfly(Color side, Move m, int bonus) {
  this->butterflyHistory[side][sq_combo_idx(m)] +=
    bonus - this->butterflyHistory[side][sq_combo_idx(m)] * abs(bonus) / 7000;
}

void History::update_continuation(int ply, Piece pc, Square sq, int bonus) {
  this->continuationHistory[ply][pc][sq] +=
    bonus - this->continuationHistory[ply][pc][sq] * abs(bonus) / 25000;
}

void History::update_captures(Piece pc, Square to, PieceType cap, int bonus) {
  this->captureHistory[pc][to][cap] +=
    bonus - captureHistory[pc][to][cap] * abs(bonus) / 10000;
}

}
