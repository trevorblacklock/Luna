# pragma once

# include "types.h"

namespace Luna {

// pv line stores an array and size of the principle variation moves
struct PvLine {
  Move pv[MAX_INTERNAL_PLY + 1];
  uint16_t length = 0;

  Move& operator()(int depth);
  Move operator()(int depth) const;
};
// pv table uses multiple pv line structs to construct a triangular table for multiple ply
struct PvTable {
  PvLine pvs[MAX_INTERNAL_PLY + 1];

  PvLine& operator()(int depth);
  PvLine operator()(int depth) const;

  void reset();
  void update(int ply, Move m);
};
}
