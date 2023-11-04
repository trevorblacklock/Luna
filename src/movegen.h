# pragma once

# include "position.h"
# include "attacks.h"
# include "history.h"

namespace Luna {

enum MovePick {
  HASH_MOVE, GEN_CAPTURES, GET_GOOD_CAPTURES,
  GET_KILLER1, GET_KILLER2, GEN_QUIETS, GET_QUIETS,
  GET_BAD_CAPTURES, GET_EVASIONS, GET_LEGAL
};

enum GenModes {
  PV_SEARCH, QSEARCH, QSEARCH_CHECKS
};

enum GenStages {
  CAPTURES,
  QUIETS,
  EVASIONS,
  LEGAL
};

struct MoveList {
  Move moves[MAX_MOVES] = {0};
  int scores[MAX_MOVES] = {0};
  int size;
};

class MoveGen {
private:
// arrays to store moves and scores
MoveList captures;
MoveList quiets;
MoveList searched;
// stores see scores
int seeScores[MAX_MOVES] = {0};
// updates see score for current move
// this is so we don't re-evaluate the see eval
int currSee;
// number of captures we consider good or equal
int goodCapNum;
// indexes for when we loop through move lists
int captureIdx;
int quietIdx;
int searchedIdx;
// store important info for move ordering
Color clr;
int stage;
int mode;
Position *psn;
History *hst;
bool skip; // if we should skip quiet moves or not
Move ttMove;
Move killers[2];
HistoryMove previousMove;
HistoryMove followupMove;

public:
void init(Position *p);
void init(Position *p, History *h, int ply, Move hash,
          HistoryMove previous, HistoryMove followup, int md);

Move next(bool underpromo = true);
Move next_capture();
Move next_quiet();
void add_move(Move m, int type);

void gen_captures(bool underpromo = true);
void gen_quiets();
void gen_evasions();
// used to update move histories
void add_searched(Move m);

template <int type>
void generate(bool underpromo = true);

// used for perft
int leaf_size() {return captures.size + quiets.size;}

// used to not regen see
int get_see() {return currSee;}

// functions related to skipping quiet moves
void skip_quiets() {skip = true;}
bool can_skip() const {return skip;}

// function to update histories
void update_history(int depth, int bestScore, int beta);
};
}
