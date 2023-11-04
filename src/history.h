# pragma once

# include "position.h"

namespace Luna {

// struct only used for history moves since seperating piece and move is confusing
struct HistoryMove {
  Move m;
  Piece pc;
};

// used for getting previous move and piece and returning as a HistoryMove
static inline HistoryMove get_previous_historymove(Position *pos, int ply = 0) {
  return HistoryMove{pos->get_previous_move(ply), pos->get_previous_piece(ply)};
}

// get the index for butterfly moves
static inline int sq_combo_idx(Move m) {
  assert(check_move(m));
  int combo = static_cast<int>(m) & 0xFFF;
  return combo;
}

struct History {
  // effort spent
  int64_t spentEffort[SQ_NB][SQ_NB] = {0};
  // eval improvement
  int maxImprovement[SQ_NB][SQ_NB] = {0};
  // killer moves, indexed with the ply and side to move
  Move killers[COLOR_NB][MAX_INTERNAL_PLY + 2][2] = {0};
  // butterfly moves, indexed with side from and to
  int butterflyHistory[COLOR_NB][SQ_NB * SQ_NB] = {0};
  // capture history, indexed with piece to and piece type
  int captureHistory[PIECE_NB][SQ_NB][PIECE_TYPE_NB] = {0};
  // counter move history, indexed with piece and to
  Move counterHistory[PIECE_NB][SQ_NB] = {0};
  // followup, indexed with piece and to
  Move followupHistory[PIECE_NB + 1][SQ_NB] = {0};
  // eval history, indexed by ply and side
  int evalHistory[COLOR_NB][MAX_INTERNAL_PLY] = {0};
  // best move
  Move bestMove = MOVE_NONE;

  // function to return a history score for a move
  int get_history(Position *p, Move m, HistoryMove previous, HistoryMove followup) const;
  // functions to get moves from the structure
  Move get_killer(Color side, int ply, int id) const;
  int get_butterfly(Color side, Move m) const;
  int get_capture_hist(Piece pc, Square to, PieceType pt) const;
  Move get_counter_hist(Piece pc, Square to) const;
  Move get_followup_hist(Piece pc, Square to) const;
  int64_t get_spent_effort(Square s1, Square s2) const;
  int get_eval_hist(Color side, int ply) const;
  int get_max_improvement(Square from, Square to) const;
  // functions to deal with clearing and setting values
  void clear();
  void set_killer(Color side, Move m, int ply);
  bool is_killer(Color side, Move m, int ply);
  void set_eval_hist(Color side, int eval, int ply);
  void set_max_improvement(Square from, Square to, int value);
  void set_capture_hist(Position *p, Move m, int val);
  void reset_killers(Color side, int ply);
  // checks against historic eval and returns if it is improving
  bool is_improving(Color side, int eval, int ply) const;
  // functions to update variable histories
  void update_butterfly(Color side, Move m, int val);
  void update_captures(Piece moved, Square to, PieceType cap, int val);
};

}
