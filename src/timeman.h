# pragma once

# include "types.h"
# include "history.h"
# include "bitboard.h"
# include "timer.h"

namespace Luna {

struct Limits {
  bool enabled = false;
  U64 val;
};

// Class for storing all information relevant to timing a search
class TimeMan {
public:
  // store all different limit types
  Limits depth_limit;
  Limits node_limit;
  Limits move_time_limit;
  Limits match_time_limit;
  // store move overhead in ms per move
  int move_overhead = 0;
  // force stop of search
  bool stop;
  // keep track of the start time of the search
  U64 start_time;

  TimeMan();

  // functions to access and setup the limits
  void set_depth_limit(int depth);
  void set_node_limit(U64 nodes);
  void set_move_time_limit(U64 time);
  void set_match_time_limit(U64 time, U64 inc, int mtg);
  // setup the start time
  void set_start_time();
  // resets the limits
  void reset();
  // get the total time
  U64 elapsed_time() const;
  // setup the move overhead
  void set_move_overhead(U64 time);
  // stop the search
  void stop_search();
  // check if the search is stopped or about to be stopped
  bool can_continue();
  // check if the search has enough time for the next iterative deepening iteration
  bool time_to_iterate(int score, int standpat);
};
}
