# pragma once

# include "position.h"

namespace Luna {

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
  // continuation history, from Stockfish
  int continuationHistory[MAX_INTERNAL_PLY + 7][PIECE_NB][SQ_NB] = {0};
  // capture history, indexed with piece to and piece type
  int captureHistory[PIECE_NB][SQ_NB][PIECE_TYPE_NB] = {0};
  // eval history, indexed by ply and side
  int evalHistory[COLOR_NB][MAX_INTERNAL_PLY] = {0};
  // best move
  Move bestMove = MOVE_NONE;

  // function to return a history score for a move
  int get_history(Position *p, Move m) const;
  // functions to get moves from the structure
  Move get_killer(Color side, int ply, int id) const;
  int get_butterfly(Color side, Move m) const;
  int get_capture_hist(Piece pc, Square to, PieceType pt) const;
  int get_continuation_hist(Piece pc, Square sq, int ply) const;
  int64_t get_spent_effort(Square s1, Square s2) const;
  int get_eval_hist(Color side, int ply) const;
  int get_max_improvement(Square from, Square to) const;
  // functions to deal with clearing and setting values
  void clear();
  void set_killer(Color side, Move m, int ply);
  bool is_killer(Color side, Move m, int ply);
  void set_eval_hist(Color side, int eval, int ply);
  void set_continuation_hist(Piece pc, Square sq, int ply, int val);
  void set_max_improvement(Square from, Square to, int value);
  void set_capture_hist(Position *p, Move m, int val);
  void set_spent_effort(Square s1, Square s2, int64_t val);
  void reset_killers(Color side, int ply);
  // checks against historic eval and returns if it is improving
  bool is_improving(Color side, int eval, int ply) const;
  // adaptively update the histories
  void update_butterfly(Color side, Move m, int bonus);
  void update_continuation(int ply, Piece pc, Square sq, int bonus);
  void update_captures(Piece pc, Square to, PieceType cap, int bonus);
  void update_spent_effort(Square s1, Square s2, int64_t val);
};

}
