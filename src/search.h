# pragma once

# include "movegen.h"
# include "position.h"
# include "types.h"
# include "timer.h"
# include "timeman.h"
# include "nn/evaluator.h"
# include "pv.h"

namespace Luna {

struct RootMove {
  // store data relevant to root moves in the search tree
  Move m;

  bool operator==(const Move& move) { return move == m; }

  int averageScore = -VALUE_INFINITE;
  int score = -VALUE_INFINITE;
  int prevScore = -VALUE_INFINITE;
  int seldepth = 0;

  // quick constructor
  RootMove(Move move)  {this->m = move; }
};

struct SearchInfo {
  // store data relevant to the previous search
  U64   nodes;
  int   depth;
  int   elapsed;
  int   score;
  int   seldepth;
  bool  timeout;
  Move  best;
};

struct SearchData {
  // store data relevant to the search of each thread
  int         id;
  int         ply;
  int         rootDepth;
  int         rootDelta;
  int         nmpMinPly;
  bool        ttPv[MAX_INTERNAL_PLY + 1] = {0};
  Color       nmpSide;
  Move        extMove;
  PvTable     pvTable;
  SearchInfo  searchInfo;
  History     historyData;
  int         doubleExtensions[MAX_INTERNAL_PLY + 1] = {0};
  MoveGen     moveGen[MAX_INTERNAL_PLY + 1];

  std::vector<RootMove> rootMoves;

  SearchData();
  explicit SearchData(int ID);
};

extern int LMR[MAX_INTERNAL_PLY][MAX_MOVES];
void init_lmr();

class Search {
private:
// store thread count and thread objects along with data associated with each threads search
int threadcnt = 1;
bool infoStrings = true;
std::vector<std::thread> runningths;
std::vector<SearchData> ths;
std::vector<RootMove> rootMoves;
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
void print_info_string(Search *search, int depth, int score, PvLine &pv, int bound = BOUND_NONE);
}
