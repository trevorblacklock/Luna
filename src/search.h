# pragma once

# include "movegen.h"
# include "position.h"
# include "types.h"
# include "timer.h"
# include "timeman.h"
# include "nn/evaluator.h"

namespace Luna {

// pv line stores an array and size of the principle variation moves
struct pvLine {
  Move pv[MAX_INTERNAL_PLY + 1];
  uint16_t length = 0;

  Move& operator()(int depth);
  Move operator()(int depth) const;
};
// pv table uses multiple pv line structs to construct a triangular table for multiple ply
struct pvTable {
  pvLine pvs[MAX_INTERNAL_PLY + 1];

  pvLine& operator()(int depth);
  pvLine operator()(int depth) const;

  void reset();
  void update(int ply, Move m);
};

extern int LMR[MAX_INTERNAL_PLY][MAX_MOVES];
void init_lmr();

// thread data struct, so threads don't all use the same data when looking at different boards
struct SearchData {
  // thread id and search data
  int id = 0;
  U64 nodes = 0;
  int seldepth = 0;
  bool timeout = false;
  // store a movelist so a new one is not always created
  MoveGen moves[MAX_INTERNAL_PLY];
  // store the pv for each thread
  pvTable pv{};
  // store the history for each thread
  History historyData{};

  // conditions for null move pruning
  int nmpSide = 2;
  int nmpMinPly = 0;

  // move that is given extensions
  Move extMove = MOVE_NONE;
  int doubleExtensions[MAX_INTERNAL_PLY];
  int rootDepth = 0;

  SearchData();
  explicit SearchData(int ID);
};

struct SearchInfo {
  // store data relevant to the previous search
  U64 nodes;
  int depth;
  int elapsed;
  int score;
  Move best;
};

class Search {
private:
// store thread count and thread objects along with data associated with each threads search
int threadcnt = 1;
bool infoStrings = true;
std::vector<std::thread> runningths;
std::vector<SearchData> ths;
public:
TimeMan* timeMan;
SearchInfo searchInfo;
// set info strings on or off
void enable_info_strings() {infoStrings = true;}
void disable_info_strings() {infoStrings = false;}
// set the amount of threads we want to use, limited by hardware number of threads
void set_threads(int cnt);
// stop the threads from searching
void stop();
// functions for the search
Move search(Position* pos, TimeMan *tm, int id = 0);
int alphabeta(Position *pos, SearchData *sd, int alpha, int beta, int depth, bool cutNode);
int qsearch(Position *pos, SearchData *sd, int alpha, int beta, bool ispv);
U64 perft(Position *pos, Depth depth, bool root = true);
// utility functions
U64 get_nodes() const;
int get_seldepth() const;
void clear_hist();
};
// used for printing info while searching
void print_info_string(Search *search, int depth, int score, pvLine &pv);
}
