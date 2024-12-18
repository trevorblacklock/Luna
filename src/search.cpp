# include "search.h"
# include "tt.h"

# include <cstring>
# include <algorithm>

namespace Luna {

// tuning variables
// only temporary
#ifndef FUTILITY_MARGIN
#define FUTILITY_MARGIN 80
#endif

#ifndef RAZOR_MARGIN
#define RAZOR_MARGIN 190
#endif

#ifndef HISTORY_LMR1
#define HISTORY_LMR1 9000
#endif

#ifndef HISTORY_LMR2
#define HISTORY_LMR2 3600
#endif

#ifndef HISTORY_LMR3
#define HISTORY_LMR3 4000
#endif

void update_continuation_histories(History *hd, Position *pos, int ply, Piece pc, Square to, int bonus);
void update_quiet_stats(History *hd, Position *pos, int ply, Move m, int bonus);
void update_history(History *hd, Position *pos, MoveGen *mg, int ply, Move bestMove, int depth);

int stat_bonus(Depth d) { return std::min(357 * d - 483, 1511); }

// info string stuff
void print_info_string(Search *search, int depth, int score, PvLine &pv, int bound) {
  U64 nodes = search->get_nodes();
  int seldepth = search->get_seldepth();
  U64 time = search->timeMan->elapsed_time();
  U64 nps = (nodes * 1000) / (time + 1);
  U64 hashfull = TT.hashfull();

  std::cout << "info" << " depth " << depth << " seldepth " << seldepth;
  // convert score to uci compatible form
  if (abs(score) > VALUE_MATE_IN) {
    std::cout << " score mate " << (VALUE_MATE - abs(score) + 1) / 2 * (score > 0 ? 1 : -1);
  }
  else std::cout << " score cp " << score;
  // print bound if it exists
  if (bound) {
    std::cout << (bound == BOUND_LOWER ? " lowerbound" : " upperbound");
  }
  std::cout << " nodes " << nodes << " nps " << nps << " time " << time << " hashfull " << hashfull;
  std::cout << " pv";
  // if our pv line didn't get saved, usually in the case of an overflow, just print the bestmove instead
  for (int i = 0; i < pv.length; i++) std::cout << " " << move(pv.pv[i]);
  std::cout << std::endl;
}

// initialize LMR for searches
int LMR[MAX_INTERNAL_PLY][MAX_MOVES];

void init_lmr() {
  for (int d = 0; d < MAX_INTERNAL_PLY; d++)
    for (int m = 0; m < MAX_MOVES; m++) {
      if (d == 0 || m == 0) {
        LMR[d][m] = 0;
        continue;
      }
      LMR[d][m] = 1.25 + log(d) * log(m) * 100 / 300;
  }
}

int reductions(int depth, int legalMoves, int delta, int rootDelta) {
  return (LMR[depth][legalMoves] + 1.5 - delta / rootDelta);
}

// setup threads given a number
void Search::set_threads(int cnt) {
  int max = std::thread::hardware_concurrency();
  // if we cannot determine a max set it to one for safety
  if (max == 0) max = 1;
  cnt = std::max(1, cnt);
  cnt = std::min(max, cnt);
  threadcnt = cnt;
  // clear previous threads
  ths.clear();
  // create new threads
  for (int i = 0; i < cnt; i++) ths.emplace_back();
}

// main search function to be called from UCI
// sets up threads and calls them to run alpha beta function
// returns best move found from the search, starts and stops all threads
Move Search::search(Position *pos, TimeMan *tm, int id) {
  // setup the search depth if given, else set it to a max
  int searchDepth = MAX_PLY - 1;
  if (tm->depth_limit.enabled)
    searchDepth = std::min(MAX_PLY - 1, (int)tm->depth_limit.val);
  // if this function is called with the main thread (from UCI) then setup other threads
  // also setup base search paramaters and clear thread data for all threads
  if (id == 0) {
    // store root moves
    MoveGen mg;
    mg.init(pos);
    Move m;

    // loop through the root moves in this position and store them
    while ((m = mg.next())) {
      ths[0].rootMoves.emplace_back(RootMove(m));
    }
    // setup the time manager
    timeMan = tm;
    // setup TT for new search
    TT.new_search();
    // reset each thread
    for (size_t i = 0; i < ths.size(); i++) {
      ths[i] = ths[0];
      ths[i].id = i;
      ths[i].searchInfo.nodes = 0;
      ths[i].searchInfo.seldepth = 0;
      ths[i].searchInfo.timeout = false;
    }

    // setup this function to be called with all other threads except the main one
    for (int t = 1; t < threadcnt; t++) {
      runningths.emplace_back(&Search::search, this, pos, tm, t);
    }
  }

  // setup score
  int score = -VALUE_INFINITE;
  // create new thread object for each thread
  SearchData *sd = &ths[id];
  // create new position object using the one passed to this function
  // this is for the threads to use individually in their search so nothing overlaps
  Position newPos{*pos};
  // set the force stop to false
  sd->searchInfo.timeout = false;
  // start iterative deepening
  int d;
  int average = -VALUE_INFINITE;

  for (d = 1; d <= searchDepth; d++) {
    int failedHigh = 0;
    // reset the pv table
    sd->pvTable.reset();
    // reset the seldepth
    sd->searchInfo.seldepth = 0;
    // use aspiration windows to speed up searches based on score recieved
    // give a size for our aspiration window to change
    // setup alpha beta and depth adjustment
    int delta = 15 + average * average / 10000;
    int alpha = std::clamp(average - delta, -(int)VALUE_INFINITE, (int)VALUE_INFINITE);
    int beta = std::clamp(average + delta, -(int)VALUE_INFINITE, (int)VALUE_INFINITE);

    // continue with iterative deepening search until time is out
    while (tm->can_continue()) {
      // set a new depth and update the searches root depth
      int newDepth = std::max(1, d - failedHigh);
      // adjust the root depth of the search
      sd->rootDepth = newDepth;
      sd->rootDelta = beta - alpha;

      // find the bestmove if it exists
      if (sd->historyData.bestMove != MOVE_NONE && score != -VALUE_INFINITE) {
        RootMove& rm = *std::find(sd->rootMoves.begin(), sd->rootMoves.end(), sd->historyData.bestMove);
        average = rm.averageScore;
      }

      // run the search
      score = alphabeta(&newPos, sd, alpha, beta, newDepth, false);

      // set previous scores in root moves
      for (RootMove& rm : sd->rootMoves)
        rm.prevScore = rm.score;

      // check for a force stop
      if (tm->stop) break;

      // when fail low/high give an update
      if (  this->infoStrings
        &&  id == 0
        &&  (score <= alpha || score >= beta)
        && tm->elapsed_time() >= 3000) {

        print_info_string(this, d, score, sd->pvTable(0), score >= beta ? BOUND_LOWER : BOUND_UPPER);
      }

      if (score <= alpha) {
        beta = (alpha + beta) / 2;
        alpha = std::max(score - delta, -(int)VALUE_INFINITE);

        failedHigh = 0;
      }

      else if (score >= beta) {
        beta = std::min(score + delta, (int)VALUE_INFINITE);
        failedHigh++;
      }

      else break;

      delta += delta / 3;

      assert(alpha >= -VALUE_INFINITE && beta <= VALUE_INFINITE);
    }

    // compute a score to calculate the time left
    int timeScore = sd->historyData.spentEffort[from_sq(sd->historyData.bestMove)][to_sq(sd->historyData.bestMove)]
                  * 100 / std::max((U64)1ULL, this->get_nodes());

    int evalScore = average - score;

    if (!tm->time_to_iterate(timeScore, evalScore)) {
      // now break out of loop
      break;
    }

    if (this->infoStrings && id == 0) {
      print_info_string(this, d, score, sd->pvTable(0));
    }
  }
  // when main thread is done return best move
  if (id == 0) {
    // stop all other threads if they haven't already finished
    tm->stop_search();
    for (std::thread& th : runningths) th.join();
    // return bestmove found in the search
    Move best = sd->historyData.bestMove;
    // clear threads once they are finished
    runningths.clear();
    // setup search overview
    searchInfo.nodes = this->get_nodes();
    searchInfo.depth = d;
    searchInfo.score = score;
    searchInfo.elapsed = timeMan->elapsed_time();
    searchInfo.best = best;
    // end
    return best;
  }
  return MOVE_NONE;
}

int Search::alphabeta(Position *pos, SearchData *sd, int alpha, int beta, int depth, bool cutNode) {

  // start off by defining some important info used in search
  Color us = pos->get_side();
  bool pvNode = (beta - alpha) != 1;
  int ply = pos->get_ply();

  // reset the pv table length
  sd->pvTable(ply).length = 0;

  // go into a q-search if depth reaches zero
  if (depth <= 0 || depth >= MAX_PLY || ply >= MAX_INTERNAL_PLY)
    return qsearch(pos, sd, alpha, beta, pvNode);

  assert(ply >= 0 && ply < MAX_INTERNAL_PLY);
  assert(alpha >= -VALUE_INFINITE && alpha < beta && beta <= VALUE_INFINITE);
  assert(depth >= 0 && depth < MAX_PLY);

  // force a stop for node limit
  if (timeMan->node_limit.enabled && timeMan->node_limit.val <= sd->searchInfo.nodes) {
    timeMan->stop_search();
  }

  // check for a force stop
  if (timeMan->stop) {
    sd->searchInfo.timeout = true;
    // clamp beta to be within search bounds
    return beta;
  }

  // if time is out we fail high to stop the search and we only check every 1024 nodes
  if (sd->searchInfo.nodes % 1024 == 0 && sd->id == 0 && !timeMan->can_continue()) {
    sd->searchInfo.timeout = true;
    timeMan->stop_search();
    // clamp beta to be within search bounds
    return beta;
  }

  // update the seldepth
  if (ply > sd->searchInfo.seldepth)
    sd->searchInfo.seldepth = ply;

  // increment nodes
  sd->searchInfo.nodes++;

  // check if there is a move that draws by repetition
  if (ply && alpha < VALUE_DRAW && pos->has_game_cycle(ply)) {
    alpha = 8 - (sd->searchInfo.nodes & 0xF);
    if (alpha >= beta) {
      return alpha;
    }
  }

  // get all the search info needed
  History*     hd         = &sd->historyData;
  U64          key        = pos->key();
  int          bestScore  = -VALUE_INFINITE;
  int          score      = -VALUE_INFINITE;
  Move         bestMove   = MOVE_NONE;
  Move         ttMove     = MOVE_NONE;
  int          standpat   = VALUE_NONE;
  int          eval       = VALUE_NONE;
  int          betaCut    = VALUE_NONE;
  bool         found      = false;
  bool         inCheck    = pos->checks();
  bool         improving  = false;

  // check for a draw and do mate distance pruning
  if (ply) {

    // draw randomization
    if (pos->is_draw() || ply >= MAX_INTERNAL_PLY) {
      return (ply >= MAX_INTERNAL_PLY && !inCheck) ? pos->evaluate() : 8 - (sd->searchInfo.nodes & 0xF);
    }

    // mate distance pruning
    alpha = std::max(mated_in(ply), alpha);
    beta = std::min(mate_in(ply + 1), beta);
    if (alpha >= beta) return alpha;
  }

  hd->reset_killers(us, ply);
  sd->cutoffCnt[ply + 2] = 0;

  // set double extensions for singular move extensions
  if (ply) sd->doubleExtensions[ply] = sd->doubleExtensions[ply - 1];

  // probe the transposition table for any exisiting entries
  // so we don't have to re-search the same position
  TTentry* tten = TT.get(key, found);
  int ttScore = found ? score_from_tt(tten->score(), ply) : VALUE_NONE;
  ttMove = found ? (Move)tten->move() : (Move)MOVE_NONE;

  assert(ttScore == VALUE_NONE || (ttScore < VALUE_INFINITE && ttScore > -VALUE_INFINITE));

  // update if move has been on the pv only if there is no extension move
  if (!sd->extMove) sd->ttPv[ply] = pvNode || (found && tten->is_pv());

  // use the score at the given entry
  // only check for a cutoff if the node is not pv
  if ( found
    && !pvNode && !sd->extMove
    && tten->depth() > depth
    && ttScore != VALUE_NONE
    && (tten->bound() & (ttScore >= beta ? BOUND_LOWER : BOUND_UPPER))) {

    // if the tt move is quiet can update the move histories
    if (ttMove) {
      if (ttScore >= beta && !pos->is_capture(ttMove) && !pos->is_promotion(ttMove))
        update_quiet_stats(hd, pos, ply, ttMove, stat_bonus(depth));
      else if (!pos->is_capture(ttMove) && !pos->is_promotion(ttMove))
        update_continuation_histories(hd, pos, ply, pos->pc_sq(from_sq(ttMove)), to_sq(ttMove), -stat_bonus(depth));
    }
    // so long as 50 move rule is not close can return the score
    if (pos->fifty() < 90) return ttScore;
  }

  if (inCheck) {
    standpat = eval = -VALUE_MATE + ply;
    improving = false;
    goto moves_loop;
  }
  else if (found) {
    standpat = eval = tten->eval();
    if (eval == VALUE_NONE)
      eval = standpat = pos->evaluate();
    if (ttScore != VALUE_NONE && (tten->bound() & (ttScore >= beta ? BOUND_LOWER : BOUND_UPPER)))
      eval = ttScore;
  }
  else {
    standpat = eval = pos->evaluate();
  }

  // setup improvement across plies
  if (ply && pos->get_previous_move() != MOVE_NONE) {
    if (hd->get_eval_hist(!us, ply - 1) > VALUE_TB_LOSS) {
      int improvement = -standpat - hd->get_eval_hist(!us, ply - 1);
      hd->set_max_improvement(from_sq(pos->get_previous_move()),
                              to_sq(pos->get_previous_move()), improvement);
    }
  }

  // set the historic eval before we adjust it using the TT
  hd->set_eval_hist(us, standpat, ply);
  improving = hd->is_improving(us, standpat, ply);

  // razoring, if the eval is low we can check if it exceeds alpha
  if (depth <= 3
    && eval + RAZOR_MARGIN * depth < beta
    && !sd->extMove
    && !pvNode) {
    score = qsearch(pos, sd, alpha, beta, pvNode);
    if (score < beta) return score;
  }

  // futility pruning
  if (   !sd->ttPv[ply]
      && !sd->extMove
      && depth < 9
      && eval - (depth - improving) * FUTILITY_MARGIN >= beta
      && eval < VALUE_TB_WIN
      && !ttMove)
    return eval;

  // null move pruning search
  if (   !pvNode
      && eval < VALUE_KNOWN_WIN
      && !sd->extMove
      && pos->get_current_move() != MOVE_NONE
      && eval >= standpat
      && eval >= beta
      && standpat >= beta - 15 * depth - (improving * 200)
      && pos->non_pawn_mat(us)
      && beta > -VALUE_KNOWN_WIN
      && ply >= sd->nmpMinPly) {

    // setup depth adjustments
    int nmpReduction = depth / 4 + 3;
    if (eval - beta < 300) nmpReduction = (eval - beta) / FUTILITY_MARGIN;

    int nullDepth = depth - nmpReduction - 3;

    pos->do_null_move();
    int v = -alphabeta(pos, sd, -beta, 1 - beta, nullDepth, !cutNode);
    pos->undo_null_move();

    // check for beta cutoff
    if (v >= beta && v < VALUE_TB_WIN) {
      // prune at low depths
      if (sd->nmpMinPly || depth < 14) return v;

      assert(!sd->nmpMinPly); // cannot verify recursively

      // adjust nmp ply and side before re-search
      sd->nmpMinPly = ply + 3 * (depth - nmpReduction) / 4;
      sd->nmpSide = us;

      // do a re-search
      int re = alphabeta(pos, sd, beta - 1, beta, nullDepth, false);
      // re-adjust nmp ply
      sd->nmpMinPly = 0;
      if (re >= beta) return v;
    }
  }

  // internal iterative reductions
  // if no tt move then reduce depth
  if (depth >= 4 && !ttMove && pvNode) {
    depth -= 2;
  }

  // probcut here is based on the implementation in Stockfish
  betaCut = beta + 180 - 60 * improving;
  if (!pvNode
    && depth > 4
    && !inCheck
    && !sd->extMove
    && abs(beta) < VALUE_TB_WIN
    && !(tten->depth() >= depth - 3 && ttScore != VALUE_NONE && ttScore < betaCut)) {

    // init the generator
    MoveGen* mg = &sd->moveGen[ply];
    mg->init(pos, hd, ply, MOVE_NONE, QSEARCH);

    assert(betaCut < VALUE_INFINITE && betaCut > -VALUE_INFINITE);

    // loop through moves
    Move m;
    while ((m = mg->next(true))) {

      if (m == sd->extMove)
        continue;

      if (!pos->is_legal(m))
        continue;

      // make the move
      pos->do_move(m, true);
      // do a qsearch
      int qScore = -qsearch(pos, sd, -betaCut, -betaCut + 1, false);

      // if the qScore is greater than betaCut then we do a full search
      if (qScore >= betaCut)
        qScore = -alphabeta(pos, sd, -betaCut, -betaCut + 1, depth - 4, !cutNode);

      // undo the move
      pos->undo_move(m);

      // check the score again
      if (qScore >= betaCut) {
        // store the score and move in the TT and return beta
        TT.save(key, depth - 3, score_to_tt(qScore, ply), hd->get_eval_hist(us, ply), m, BOUND_LOWER, sd->ttPv[ply]);
        return qScore;
      }
    }
  }

moves_loop:

  // setup movegen
  MoveGen* mg = &sd->moveGen[ply];
  mg->init(pos, hd, ply, ttMove, PV_SEARCH);
  Move m;

  int legalMoves = 0;
  int moveCnt    = 0;

  // loop through all the pseudo-legal moves until none remain,
  // legality is checked before playing a move rather than before
  // to increase the performance when looping through large lists
  while ((m = mg->next(true))) {

    // check if we should skip this move
    if (sd->extMove == m) continue;

    // check move for legality
    if (!pos->is_legal(m))
      continue;

    // get some info about the move
    Square from = from_sq(m);
    Square to = to_sq(m);
    Piece pc = pos->pc_sq(from);
    bool isCapture = pos->is_capture(m);
    bool isPromotion = pos->is_promotion(m);
    bool givesCheck = pos->gives_check(m);
    bool quiet = !isCapture && !isPromotion && !givesCheck;
    int newDepth = depth - 1;
    int extension = 0;

    // increment the move counter
    moveCnt++;

    // send currmove info
    if (ply == 0 && sd->id == 0 && this->infoStrings && timeMan->elapsed_time() >= 3000) {
      std::cout << "info depth " << sd->rootDepth << " currmove "
                << move(m) << " currmovenumber " << moveCnt << std::endl;
    }

    int delta = beta - alpha;
    int r = reductions(depth, legalMoves, delta, sd->rootDelta);

    if (ply && pos->non_pawn_mat(us) && legalMoves && bestScore > VALUE_TB_LOSS) {
      // find a crude depth estimation
      int moveDepth = std::max(1, 1 + depth - r);

      if (quiet) {

        // if we have searched enough quiet moves skip this move
        if (mg->can_skip()) continue;

        int quietPruning = improving ? (3 + depth * depth) : (3 + depth * depth) / 2;

        // if the number of quiets searched passes a threshold
        // set a flag to not search any more quiet moves
        if (moveCnt >= quietPruning) mg->skip_quiets();

        // prune quiet moves that are unlikely to improve alpha
        if (!inCheck
          && moveDepth <= 7
          && hd->get_max_improvement(from, to)
            + moveDepth * FUTILITY_MARGIN + 100
            + hd->get_eval_hist(us, ply) < alpha)
          continue;

        int history = hd->get_continuation_hist(pc, to, ply - 1)
                    + hd->get_continuation_hist(pc, to, ply - 2);

        if (history < -4000 * depth) continue;
      }

      // prune the move if it has a low see value
      if (moveDepth <= 5 + quiet * 3
        && piece_type(pc) < piece_type(pos->pc_sq(to))
        && (isCapture ? mg->get_see() : pos->see_eval(m))
        <= (quiet ? -40 * moveDepth : -100 * moveDepth))
        continue;
    }

    // set the spent effort
    if (!ply && depth == 1) {
      hd->set_spent_effort(from, to, 0);
    }

    // singular move extensions
    // make sure cannot extend too far
    if (ply < sd->rootDepth * 2) {
      if ( ply
        && m == ttMove
        && !sd->extMove
        && depth >= 4 + 2 * (pvNode && tten->is_pv())
        && abs(ttScore) < VALUE_TB_WIN
        && (tten->bound() & BOUND_LOWER)
        && tten->depth() >= depth - 3) {

        // init values of singular beta and singular depth
        int sBeta = ttScore - (50 + 50 * (sd->ttPv[ply] && !pvNode)) * depth / 50;
        int sDepth = (depth - 1) / 2;

        assert(sBeta > -VALUE_INFINITE && sBeta < VALUE_INFINITE);

        // set the extension move as to not prune or enter another extension loop with it
        sd->extMove = m;
        score = alphabeta(pos, sd, sBeta - 1, sBeta, sDepth, cutNode);
        sd->extMove = MOVE_NONE;

        if (score < sBeta) {
          extension = 1;

          if (!pvNode
            && score < sBeta
            && sd->doubleExtensions[ply] < 12) {
            extension = 2;
            depth += depth < 12;
          }
        }

        // if subtree still fails high with ttMove being excluded
        // can safely return singular beta since ttMove already fails high
        else if (sBeta >= beta)
          return sBeta;

        // negative extensions
        else if (ttScore >= beta)
          extension = -2 - !pvNode;

        // if the ttMove fails low
        else if (ttScore <= score)
          extension = -1;

        mg->init(pos, hd, ply, ttMove, PV_SEARCH);
        m = mg->next(true);
      }
    }

    newDepth += extension;
    if (ply) sd->doubleExtensions[ply] = sd->doubleExtensions[ply - 1] + (extension == 2);

    // make the move
    pos->do_move(m, true);

    // keep track of the amount of nodes
    U64 nodeCount = sd->searchInfo.nodes;

    if (sd->ttPv[ply])
      r -= 1 + (tten->score() > alpha) + (tten->depth() >= depth);

    // reduce reduction at pvnode
    r -= pvNode;

    // increase reduction at cut nodes
    if (cutNode)
      r += 2 - (tten->depth() >= depth && sd->ttPv[ply])
             + (!sd->ttPv[ply] && m != ttMove && hd->is_killer(us, ply, m));

    // increase reduction if position not improving
    r += !improving;

    if (sd->cutoffCnt[ply + 1] > 3)
      r += 1 + !(pvNode | cutNode);

    else if (m == ttMove)
      r = std::max(0, r - 2);

    // adjust the reductions based on multiple heuristics
    // first calculate the history and adjust the reduction
    int history = 2 * hd->get_butterfly(us, m)
                + hd->get_continuation_hist(pc, to, ply - 1)
                + hd->get_continuation_hist(pc, to, ply - 2) - 4000;

    r -= history / 10000;

    // setup a new depth to search with using the reductions and extensions
    // never want reductions to extend search further than 1 and
    // must clamp the value to avoid any sort of problems
    int d = std::clamp(newDepth - r, 1, newDepth + 1);

    // search the position with Late Move Reductions
    // the conditions to enter LMR are that we must be past a starting depth
    // we must have already searched a single move in the position
    // and the position cannot have been on the pv, a capture or a followup from a single move position
    if (ply
      && depth >= 2
      && moveCnt > 1) {

      // do the search with reductions applied
      score = -alphabeta(pos, sd, -alpha - 1, -alpha, d, true);

      // perform a re-search if the last search failed high and had reduced depth
      if (score > alpha && d < newDepth) {

        score = -alphabeta(pos, sd, -alpha - 1, -alpha, newDepth, !cutNode);

        // calculate a bonus to update continuation histories
        int bonus = score <= alpha ? -stat_bonus(newDepth)
                  : score >= beta ? stat_bonus(newDepth) : 0;

        if (bonus != 0)
          update_continuation_histories(hd, pos, ply, pc, to, bonus);
      }
    }

    // when the conditions for LMR are not met enter a full depth search
    // reduce this search if there is no ttmove and it is a cutnode
    else if (!pvNode || moveCnt > 1) {
      // increase reductions for cutnodes
      if (!ttMove && cutNode) r += 2;

      score = -alphabeta(pos, sd, -alpha - 1, -alpha, newDepth - (r > 3), !cutNode);

    }

    // for pv-nodes do a full re-search upon failing high in previous search
    if (pvNode && (moveCnt == 1 || score > alpha)) {
      // perform the re-search
      score = -alphabeta(pos, sd, -beta, -alpha, newDepth, false);
    }

    // undo the move and add it to searched moves
    pos->undo_move(m);
    mg->add_searched(m);

    assert(score > -VALUE_INFINITE && score < VALUE_INFINITE);

    if (!ply) {
      // use the root depth to update the average scores
      RootMove& rm = *std::find(sd->rootMoves.begin(), sd->rootMoves.end(), m);

      rm.averageScore = (rm.averageScore != -VALUE_INFINITE) ? (2 * score + rm.averageScore) / 3 : score;

      // update the score while here
      if (moveCnt == 1 || score > alpha) {
        rm.score = score;
        rm.seldepth = sd->searchInfo.seldepth;
      }
      else {
        rm.score = -VALUE_INFINITE;
      }

      // update the effort for time heuristics
      hd->update_spent_effort(from, to, sd->searchInfo.nodes - nodeCount);
    }

    // increment the legalmoves of the position
    legalMoves++;

    // check for a new bestmove by checking the current score against the historic best score
    if (score > bestScore) {
      // update the bestscore and bestmove
      bestScore = score;
      bestMove = m;

      // check for low depth search and rootnode to update alpha and global bestmove
      if (!ply && (timeMan->can_continue() || depth <= 2)) {
        hd->bestMove = m;
      }

      // update the pv at pv-nodes
      if (pvNode && sd->id == 0 && !sd->searchInfo.timeout) {
        sd->pvTable.update(ply, m);
      }

      // check for a beta-cutoff
      if (score >= beta) {

        sd->cutoffCnt[ply] += 1 + !ttMove - (extension >= 2);

        if (!pvNode && std::abs(bestScore) < VALUE_TB_WIN
            && std::abs(beta) < VALUE_TB_WIN
            && std::abs(alpha) < VALUE_TB_WIN)
          bestScore = (bestScore * depth + beta) / (depth + 1);

        // if there is no extension move save the score to the TT
        if (!sd->extMove) {
          // give the entry a lower bound since we failed low
          TT.save(key, depth, score_to_tt(bestScore, ply), hd->get_eval_hist(us, ply), m, BOUND_LOWER, sd->ttPv[ply]);
        }
        // update the move histories
        update_history(hd, pos, mg, ply, bestMove, depth);

        // return the bestscore
        return bestScore;
      }
      // check for a fail high
      else if (score > alpha) {
        // update alpha
        alpha = score;
      }
    }
  }

  // if there are no legal moves then it's either stalemate or checkmate
  // we can find out from checking if we are in check or not
  if (legalMoves == 0) bestScore = sd->extMove ? alpha : inCheck ? mated_in(ply) : VALUE_DRAW;

  assert(bestScore > -VALUE_INFINITE && bestScore < VALUE_INFINITE);

  if (bestScore <= alpha && ply)
    sd->ttPv[ply] = sd->ttPv[ply] || (sd->ttPv[ply - 1] && depth > 3);

  // write the bestscore to the TT when there is no extension move
  if (!sd->extMove && !sd->searchInfo.timeout) {
    TT.save(key, depth, score_to_tt(bestScore, ply), hd->get_eval_hist(us, ply), bestMove,
            bestMove && pvNode ? BOUND_EXACT : BOUND_UPPER, sd->ttPv[ply]);
  }

  // return the best score
  return bestScore;
}

int Search::qsearch(Position *pos, SearchData *sd, int alpha, int beta, bool pvNode) {

  assert(alpha >= -VALUE_INFINITE && alpha < beta && beta <= VALUE_INFINITE);

  // start with increasing the nodes searched
  sd->searchInfo.nodes++;

  // once again get all the useful info
  History* hd        = &sd->historyData;
  U64      key       = pos->key();
  bool     found     = false;
  int      ply       = pos->get_ply();
  int      bestScore = -VALUE_INFINITE;
  int      standpat;
  int      ttDepth   = 0;
  Move     bestMove  = MOVE_NONE;
  bool     inCheck   = pos->checks();

  // check if theres a move that causes a draw
  if (pos->is_draw() || ply >= MAX_INTERNAL_PLY) {
    // draw randomization
    return (ply >= MAX_INTERNAL_PLY&& !inCheck) ? pos->evaluate() : (sd->searchInfo.nodes & 0xF);
  }

  assert(ply >= 0 && ply < MAX_INTERNAL_PLY);

  // probe the transposition table for any exisiting entries
  // so we don't have to re-search the same position
  TTentry* tten = TT.get(key, found);
  int ttScore = found ? score_from_tt(tten->score(), ply) : VALUE_NONE;
  bool ttpv   = sd->ttPv[ply];

  assert(ttScore == VALUE_NONE || (ttScore < VALUE_INFINITE && ttScore > -VALUE_INFINITE));

  // at non-pv nodes check for an early cutoff
  if (found
      && !pvNode
      && ttScore != VALUE_NONE
      && tten->depth() >= inCheck
      && (tten->bound() & (ttScore >= beta ? BOUND_LOWER : BOUND_UPPER)))
      return ttScore;

  // statically evaluate the position
  if (inCheck) {
    bestScore = standpat = -VALUE_MATE + ply;
  }
  else {
    // check for static eval in the TT
    if (found) {

      // ensure stored value is legal and not corrupted
      if ((standpat = bestScore) == -VALUE_INFINITE)
        standpat = bestScore = pos->evaluate();

      // check if tt value can be used as better standpat
      if (ttScore != VALUE_NONE
          && (tten->bound() & (ttScore > bestScore ? BOUND_LOWER : BOUND_UPPER)))
          bestScore = ttScore;

    }
    else {
      // static evaluation of position
      standpat = bestScore = pos->evaluate();
    }

    // immediately check if static eval is above beta
    if (bestScore >= beta) {
      assert(bestScore > -VALUE_INFINITE && bestScore < VALUE_INFINITE);
      // if the tt was not hit, can write this value into the TT
      if (!found)
        TT.save(key, 0, score_to_tt(bestScore, ply), standpat, MOVE_NONE, BOUND_LOWER, false);

      return bestScore;
    }

    // if best score exceeds alpha overwrite it
    if (bestScore > alpha)
      alpha = bestScore;
  }

  // setup movegen
  MoveGen* mg = &sd->moveGen[ply];

  // initialize the move generator
  Move m;
  mg->init(pos, hd, ply, MOVE_NONE, QSEARCH + inCheck);

  int moveCnt = 0;

  // main move loop
  while ((m = mg->next(false))) {

    // check the move for legality
    if (!pos->is_legal(m))
      continue;

    // init some info about the move
    bool givesCheck = pos->gives_check(m);

    moveCnt++;

    // static exchange evaluation pruning
    // don't consider a move that is a bad capture at a low depth
    int see = (!inCheck && (pos->is_capture(m) || pos->is_promotion(m))) ? mg->get_see() : 0;
    if (see < 0) continue;
    if (see + standpat > beta + 200) return beta;

    // now make the move and search
    pos->do_move(m, true);
    int score = -qsearch(pos, sd, -beta, -alpha, pvNode);
    pos->undo_move(m);

    assert(score > -VALUE_INFINITE && score < VALUE_INFINITE);

    // check for a new best score
    if (score > bestScore) {
      // update the best score
      bestScore = score;
      // check for exceeding alpha
      if (score > alpha) {
        // update bestmove and PV if node is pv
        bestMove = m;

        // update alpha now
        if (score < beta)
          alpha = score;
        else {
          ttDepth = givesCheck;
          break;
        }
      }
    }
  }

  // check for a checkmate
  if (inCheck && bestScore == -VALUE_INFINITE) {
    assert(!mg->leaf_size());
    return mated_in(ply);
  }

  // only store reputable values in the TT
  // if there is no move count the stored values will likely just be inflated
  if (moveCnt) {
    TT.save(key, ttDepth, score_to_tt(bestScore, ply), standpat, bestMove,
            bestScore >= beta ? BOUND_LOWER : BOUND_UPPER, ttpv);
  }

  assert(bestScore > -VALUE_INFINITE && bestScore < VALUE_INFINITE);

  return bestScore;
}

U64 Search::get_nodes() const {
  U64 nodes = 0;
  for (const auto &r : ths) nodes += r.searchInfo.nodes;
  return nodes;
}

int Search::get_seldepth() const {
  int max = 0;
  for (const auto &r : ths) max = std::max(max, r.searchInfo.seldepth);
  return max;
}

// from Stockfish
// update histories of move pairs
// by moves at ply -1, -2, -3, -4 and -6 with curr move
void update_continuation_histories(History *hd, Position *pos, int ply, Piece pc, Square to, int bonus) {

  for (int i : {1, 2, 3, 4, 6}) {
    // only update first 2 if in check
    if (pos->checks() && i > 2)
      break;
    if (pos->get_previous_move(i) != MOVE_NONE) {
      hd->update_continuation(ply - i + 7, pc, to, bonus / (1 + 3 * (i == 3)));
    }
  }
}

// update the quiet statistics
void update_quiet_stats(History *hd, Position *pos, int ply, Move m, int bonus) {

  Color us = pos->get_side();

  // update killers
  // will not overwrite in case set_killers is called seperately
  if (type_of_move(m) != PROMOTION)
    hd->set_killer(us, m, ply);

  // update main histories
  hd->update_butterfly(us, m, bonus);
  update_continuation_histories(hd, pos, ply, pos->piece_moved(m), to_sq(m), bonus);
}

// update all histories upon search end
void update_history(History *hd, Position *pos, MoveGen *mg, int ply, Move bestMove, int depth) {

  Piece pc      = pos->piece_moved(bestMove);
  Square to     = to_sq(bestMove);
  PieceType cap;

  // calculate the bonus
  int bonus = stat_bonus(depth);

  if (pos->is_capture(bestMove)) {
    // increase stats for best capture move
    cap = piece_type(pos->pc_sq(to));
    hd->update_captures(pc, to, cap, bonus);
  }
  else {

    // increase stats for best quiet move
    update_quiet_stats(hd, pos, ply, bestMove, bonus);

    for (int i = 0; i < mg->searched.size - 1; i++) {
      Move m = mg->searched.moves[i];
      if (!pos->is_capture(m) && bestMove != m) {
        update_quiet_stats(hd, pos, ply, m, -bonus);
      }
    }
  }

  // decrease stats for non best capture moves
  for (int i = 0; i < mg->searched.size - 1; i++) {
    Move m = mg->searched.moves[i];
    if (pos->is_capture(m) && bestMove != m) {
      pc = pos->piece_moved(m);
      to = to_sq(m);
      cap = piece_type(pos->pc_sq(to));
      hd->update_captures(pc, to, cap, -bonus);
    }
  }
}

// perft function is a debugging tool used to verify our move generation
// the sum of all legal positions is returned and can be checked against the known amount
U64 Search::perft(Position *pos, Depth depth, bool root) {

  // initialize nodes to 0
  U64 cnt, nodes = 0;
  Move m;
  // leaf will be used to speed up perft using move legality
  // knowing all moves in the list are legal, we can increment by the size of the movelist instead
  const bool leaf = (depth == 2);

  // create new movegen object to generate all moves
  MoveGen mg;
  mg.init(pos);

  // loop through all moves
  while ((m = mg.next(true))) {
    if (root && depth <= 1)
      cnt = 1, nodes++;
    else {
      pos->do_move(m);

      if (leaf) {
        MoveGen lf;
        lf.init(pos);
        cnt = lf.leaf_size();
      }

      else cnt = perft(pos, depth - 1, false);

      nodes += cnt;
      pos->undo_move(m);
    }
    if (root)
      std::cout << move(m) << ": " << cnt << std::endl;
  }
  return nodes;
}

void Search::clear_hist() {
  for (auto &td : ths) {
    td.historyData.clear();
  }
}

void Search::stop() {
  if (timeMan)
    timeMan->stop_search();
}

SearchData::SearchData(int ID) : id(ID) {}
SearchData::SearchData() {}
}
