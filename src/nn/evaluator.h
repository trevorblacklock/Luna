#pragma once

#include "accumulator.h"

namespace Luna::NeuralNet {

class Evaluator {
public:
  std::vector<Accumulator>      history {};
  std::unique_ptr<RefreshTable> refreshTable;
  uint32_t                      historyIdx   = 0;

  // constructors and stuff
  Evaluator();
  Evaluator(const Evaluator& eval);
  Evaluator& operator=(const Evaluator& eval);

  // function to reset the accumulator
  void reset(Position *pos);
  void reset_history();

  // function to set move info into accumulator for later updates
  void update_move_history(Position *pos, Move m, Piece pc, Piece cap, bool undo);

  // functions for evaluation
  void apply_lazy_updates(Position *pos, Color side, Move m, Piece pc, Piece cap);
  int predict(Position *pos);
  int propagate(Color side);
};
}
