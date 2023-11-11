# include "search.h"
# include "tt.h"

# include <cstring>
# include <algorithm>

namespace Luna {

// info string stuff
void print_info_string(Search *search, int depth, int score, PvLine &pv) {
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
    for (int m = 0; m < MAX_MOVES; m++)
      LMR[d][m] = 1.25 + log(d) * log(m) * 100 / 300;
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
  int searchDepth = MAX_PLY;
  if (tm->depth_limit.enabled)
    searchDepth = std::min(MAX_PLY, (int)tm->depth_limit.val);
  // if this function is called with the main thread (from UCI) then setup other threads
  // also setup base search paramaters and clear thread data for all threads
  if (id == 0) {
    // setup the time manager
    timeMan = tm;
    // setup TT for new search
    TT.new_search();
    // reset each thread
    for (size_t i = 0; i < ths.size(); i++) {
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
  int score = 0;
  int prevscore = 0;
  // create new thread object for each thread
  SearchData *sd = &ths[id];
  // create new position object using the one passed to this function
  // this is for the threads to use individually in their search so nothing overlaps
  Position newPos{*pos};
  // set the force stop to false
  sd->searchInfo.timeout = false;
  // start iterative deepening
  int d;
  for (d = 1; d <= searchDepth; d++) {
    // reset the pv table
    sd->pvTable.reset();
    // use aspiration windows to speed up searches based on score recieved
    // only use them past depth 6 to not interfere with the fast initial searches
    if (d <= 6) {
      sd->rootDepth = d;
      score = alphabeta(&newPos, sd, -VALUE_INFINITE, VALUE_INFINITE, d, false);
      prevscore = score;
    }
    else {
      // give a size for our aspiration window to change
      int window = 10;
      // setup alpha beta and depth adjustment
      int alpha = std::max(score - window, -(int)VALUE_INFINITE);
      int beta = std::min(score + window, (int)VALUE_INFINITE);
      int newDepth = d;
      // continue with iterative deepening search until time is out
      while (tm->can_continue()) {
        newDepth = newDepth < d - 3 ? d - 3 : newDepth;
        sd->rootDepth = newDepth;
        score = alphabeta(&newPos, sd, alpha, beta, newDepth, false);
        window += window;
        // don't increase window past 500
        if (window > 500)
          window = VALUE_MATE_IN;
        // adjust alpha and beta depending on the search dropout
        if (score >= beta) {
          beta += window;
          newDepth--;
        }
        else if (score <= alpha) {
          beta = (alpha + beta) / 2;
          alpha -= window;
        }
        else break;
      }
    }
    if (!tm->can_continue()) break;
    if (id == 0 && this->infoStrings) {
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

int RAZOR_MARGIN      = 190;
int FUTILITY_MARGIN   = 80;
int  lmp[2][8]        = {{0, 2, 3, 5, 8, 12, 17, 23}, {0, 3, 6, 9, 12, 18, 28, 40}};

int Search::alphabeta(Position *pos, SearchData *sd, int alpha, int beta, int depth, bool cutNode) {

  Color us = pos->get_side();
  bool ispv = (beta - alpha) != 1;
  int ply = pos->get_ply();

  // force a stop for node limit
  if (timeMan->node_limit.enabled && timeMan->node_limit.val <= sd->searchInfo.nodes) {
    timeMan->stop_search();
  }

  // reset our pv table length unless ply is exceeded
  sd->pvTable(ply).length = 0;

  // check for a force stop
  if (timeMan->stop) {
    sd->searchInfo.timeout = true;
    return beta;
  }

  // if time is out we fail high to stop the search and we only check every 1024 nodes
  if (sd->searchInfo.nodes % 1024 == 0 && sd->id == 0 && !timeMan->can_continue()) {
    sd->searchInfo.timeout = true;
    timeMan->stop_search();
    return beta;
  }

  // check for a draw here
  if (ply && pos->is_draw()) {
    return 8 - (sd->searchInfo.nodes & 0xF);
  }

  // find if player is in check
  bool inCheck = pos->checks();

  // keep track of seldepth
  if (ply > sd->searchInfo.seldepth)
    sd->searchInfo.seldepth = ply;

  // check for overflows
  if (depth <= 0 || depth > MAX_PLY || ply > MAX_SEARCH_PLY)
    return qsearch(pos, sd, alpha, beta, ispv);

  if (ply) {
    alpha = std::max(mated_in(ply), alpha);
    beta = std::min(mate_in(ply + 1), beta);
    if (alpha >= beta) return alpha;
  }

  assert(ply >= 0 && ply < MAX_INTERNAL_PLY);

  // increment the nodes
  sd->searchInfo.nodes++;

  // get all the search info needed
  History*     hd        = &sd->historyData;
  U64          key       = pos->key();
  int          oalpha    = alpha;
  int          bestScore = -VALUE_INFINITE;
  int          score     = -VALUE_INFINITE;
  Move         bestMove  = MOVE_NONE;
  Move         ttMove    = MOVE_NONE;
  int          standpat  = VALUE_NONE;
  bool         found     = false;

  if (ply > 0) sd->doubleExtensions[ply] = sd->doubleExtensions[ply - 1];

  // probe the transposition table for any exisiting entries
  // so we don't have to re-search the same position
  TTentry* tten = TT.get(key, found);
  int ttScore = found ? score_from_tt(tten->score(), ply) : VALUE_NONE;
  bool canFutility = !(ispv || (found && tten->is_pv()));

  // use the score at the given entry
  // only check for a cutoff if the node is not pv
  if (found && ttScore != VALUE_NONE && !sd->extMove) {
    // set the static eval and the ttmove from the table
    ttMove = tten->move();
    standpat = tten->eval();
    // check if tt value is good enough to be returned
    if (!ispv &&
       (tten->depth() > (depth - (sd->id % 2 == 1))) &&
       (pos->fifty() < 90) &&
       ((tten->bound() == BOUND_EXACT) ||
       ((tten->bound() == BOUND_LOWER) && (ttScore >= beta)) ||
       ((tten->bound() == BOUND_UPPER) && (ttScore <= alpha)))) {

         return ttScore;
     }
  }
  else {
    if (inCheck) {
      standpat = -VALUE_MATE + ply;
    }
    else {
      standpat = pos->evaluate();
    }
  }

  // set the max improvement across plies
  if (!inCheck) {
    if (ply && pos->get_previous_move() != MOVE_NONE) {
      if (hd->get_eval_hist(!us, ply - 1) > VALUE_TB_LOSS) {
        int improvement = -standpat - hd->get_eval_hist(!us, ply - 1);
        hd->set_max_improvement(from_sq(pos->get_previous_move()),
                                to_sq(pos->get_previous_move()), improvement);
      }
    }
  }

  // set the historic eval before we adjust it using the TT
  hd->set_eval_hist(us, standpat, ply);
  bool isImproving = inCheck ? false : hd->is_improving(us, standpat, ply);

  // adjust the eval using the TT
  // be sure to not adjust the eval incorrectly and make sure eval is not VALUE_NONE
  if (found) {
    // use TT score as a better eval
    if (  (tten->bound() == BOUND_EXACT)
       || (tten->bound() == BOUND_LOWER && standpat < ttScore)
       || (tten->bound() == BOUND_UPPER && standpat > ttScore)) {
      standpat = ttScore;
    }
  }
  // ensure standpat has a value
  if (standpat == VALUE_NONE) standpat = pos->evaluate();

  // reset the past killer moves
  hd->reset_killers(us, ply);

  // if we are in check skip static pruning
  if (inCheck) {
    goto moves_loop;
  }

  // razoring, if the eval is low we can check if it exceeds alpha
  if (!ispv
    && depth <= 7
    && !sd->extMove
    && standpat < alpha - 350 - 250 * depth * depth) {
    score = qsearch(pos, sd, alpha - 1, alpha, false);
    if (score < alpha) return score;
  }

  // futility pruning
  if (   canFutility == true
      && depth < 8
      && !sd->extMove
      && standpat - (depth - isImproving) * FUTILITY_MARGIN >= beta
      && standpat < VALUE_TB_WIN
      && !(ttMove && pos->is_capture(ttMove)))
    return standpat;

  // null move pruning search
  if (   !ispv
      && standpat < VALUE_KNOWN_WIN
      && !sd->extMove
      && pos->get_current_move() != MOVE_NONE
      && standpat >= beta - 15 * depth - (isImproving * 200)
      && pos->non_pawn_mat(us)
      && (ply >= sd->nmpMinPly || us != sd->nmpSide)) {

    // setup depth adjustments
    int nmpReduction = depth / 4 + 3;
    if (standpat - beta < 300) nmpReduction = (standpat - beta) / FUTILITY_MARGIN;

    int nullDepth = depth - nmpReduction - 3;

    pos->do_null_move();
    int v = -alphabeta(pos, sd, -beta, 1 - beta, nullDepth, !cutNode);
    pos->undo_null_move();

    // check for beta cutoff
    if (v >= beta) {
      // don't return a mate score
      if (v > VALUE_KNOWN_WIN) v = beta;
      if (abs(beta) < VALUE_KNOWN_WIN && depth < 14) return v;

      // adjust nmp ply and side before re-search
      sd->nmpMinPly = ply + 3 * (depth - nmpReduction) / 4;
      sd->nmpSide = us;

      // do a re-search
      v = alphabeta(pos, sd, beta - 1, beta, nullDepth, false);
      // re-adjust nmp ply
      sd->nmpMinPly = 0;
      if (v >= beta) return v;
    }
  }

moves_loop:

  // IID by decreasing the depth
  if (depth >= 4 && !ttMove) depth--;

  // setup movegen
  MoveGen* mg = &sd->moveGen[ply];

  // probcut
  int betaCut = beta + 180 - 60 * isImproving;
  if (!inCheck
    && !ispv
    && !sd->extMove
    && depth > 4
    && !(ttMove && tten->depth() >= depth - 3 && ttScore < betaCut)) {

    // init the generator
    mg->init(pos, hd, ply, MOVE_NONE, QSEARCH);

    // loop through moves
    Move m;
    while ((m = mg->next(false))) {

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
        TT.save(key, depth - 3, score_to_tt(qScore, ply), hd->get_eval_hist(us, ply), m, BOUND_LOWER, false);
        return betaCut;
      }
    }
  }

  int legalMoves = 0;
  U64 prevNodes = sd->searchInfo.nodes;
  U64 bestNodes = 0;

  // initialize the move generator
  Move m;
  int moveCnt = 0;
  int quiets = 0;
  mg->init(pos, hd, ply, ttMove, PV_SEARCH);

  // loop through all the pseudo-legal moves until none remain,
  // legality is checked before playing a move rather than before
  // to increase the performance when looping through large lists
  while ((m = mg->next(false))) {

    // check if we should skip this move
    if (sd->extMove == m) continue;
    // get some info about the move
    Square from = from_sq(m);
    Square to = to_sq(m);
    Piece pc = pos->pc_sq(from);
    bool isCapture = pos->is_capture(m);
    bool isPromotion = pos->is_promotion(m);
    bool givesCheck = pos->gives_check(m);
    int see = isCapture ? mg->get_see() : 1;
    bool quiet = !isCapture && !isPromotion && !givesCheck;

    // check move for legality
    if (!pos->is_legal(m))
      continue;

    if (ply > 0 && legalMoves > 0 && bestScore > VALUE_TB_LOSS) {
      int moveDepth = std::max(1, 1 + depth - LMR[depth][legalMoves]);

      if (quiet) {
        quiets++;

        if (mg->can_skip()) continue;


        if (depth <= 7 && quiets >= lmp[isImproving][depth]) mg->skip_quiets();

        if (!inCheck
          && moveDepth <= 7
          && hd->get_max_improvement(from, to)
            + 100 * FUTILITY_MARGIN +
            + hd->get_eval_hist(us, ply) < alpha)
          continue;

        if (!inCheck
          && hd->get_history(pos, m)
            < std::min(140 - 300 * (depth * (depth + isImproving)), 0))
          continue;

      }

      if (moveDepth <= 5 + quiet * 3
        && isCapture
        && (see <= -200 * moveDepth))
        continue;
    }

    // basic lmr for now
    int lmr = (legalMoves < 2 - (ttMove != MOVE_NONE) + ispv || depth <= 2
           || (isCapture && see > 0)
           || (isPromotion)) ? 0 : LMR[depth][legalMoves];

    if (legalMoves && depth > 2 && us == sd->nmpSide) lmr++;

    if (lmr) {
      int hist = hd->get_history(pos, m);

      // decrease/increase lmr based on moves history
      lmr -= std::clamp(hist / 5000, -5, 5);
      // increase lmr if position is not improving
      lmr += !isImproving;
      // decrease lmr if we are in a pv node
      lmr -= ispv;
      // decrease lmr if the move is a killer
      if (hd->is_killer(us, m, ply)) lmr--;
      // increase lmr if the static eval is far from alpha
      lmr += std::min(2, std::abs(standpat - alpha) / 350);
      // increase lmr for cut nodes
      if (cutNode) lmr += 2;

      // limit the lmr
      if (lmr > MAX_PLY) lmr = 0;
      if (lmr > depth - 2) lmr = depth - 2;
    }

    moveCnt++;

    U64 nodeCount = sd->searchInfo.nodes;
    int extension = 0;

    // single move extensions
    if (ply < sd->rootDepth * 2) {
      if ( ply
        && depth >= 4 + 2 * (ispv && tten->is_pv())
        && legalMoves == 0
        && m == ttMove
        && !sd->extMove
        && abs(ttScore) < VALUE_TB_WIN
        && (tten->bound() & BOUND_LOWER)
        && tten->depth() >= depth - 3) {

        int newBeta = ttScore - (70 + 60 * !ispv) * depth / 60;
        int newDepth = (depth - 1) / 2;
        // find new score while excluding ttMove
        sd->extMove = m;
        score = alphabeta(pos, sd, newBeta - 1, newBeta, newDepth, cutNode);
        sd->extMove = MOVE_NONE;
        // check score
        if (score < newBeta) {
          extension = 1;

          if (!ispv
            && score < newBeta - 25
            && sd->doubleExtensions[ply] <= 8) {
            extension = 2;
            depth += depth < 12;
          }
        }
        else if (newBeta >= beta) {
          return newBeta;
        }
        else if (ttScore >= beta) {
          extension = -2 - !ispv;
        }
        else if (cutNode)
          extension = depth < 19 ? -2 : -1;
        else if (ttScore <= score)
          extension = -1;

        mg->init(pos, hd, ply, ttMove, PV_SEARCH);
        m = mg->next();
      }
      else if (givesCheck
            && depth > 9)
        extension = 1;

      else if (ispv
            && m == ttMove
            && hd->is_killer(us, m, ply)
            && hd->get_butterfly(us, m) >= 2000)
        extension = 1;
    }

    if (ply) sd->doubleExtensions[ply] = sd->doubleExtensions[ply - 1] + (extension == 2);

    // make the move
    pos->do_move(m, true);

    int d = depth - 1 + extension;

    // PVS search
    if (legalMoves == 0) {
      score = -alphabeta(pos, sd, -beta, -alpha, d, !cutNode);
    }
    else {
      // do a reduced search
      int rdepth = std::clamp(d - lmr, 0, d + 1);
      score = -alphabeta(pos, sd, -alpha - 1, -alpha, rdepth, true);
      // do re-searches if needed
      if (lmr && score > alpha)
        score = -alphabeta(pos, sd, -alpha - 1, -alpha, d, !cutNode);
      if (score > alpha && score < beta)
        score = -alphabeta(pos, sd, -beta, -alpha, d, !cutNode);
    }

    // undo the move and add it to searched
    pos->undo_move(m);
    mg->add_searched(m);

    // if we got a new best score we can update the pv and bestscore
    if (score > bestScore) {
      bestScore = score;
      bestMove = m;
      if ((ply == 0) && (timeMan->can_continue() || depth <= 2) && sd->id == 0) {
        alpha = bestScore;
        hd->bestMove = m;
      }
      if (ispv && sd->id == 0)
        sd->pvTable.update(ply, m);
      bestNodes = sd->searchInfo.nodes - nodeCount;
    }
    // check for a beta cutoff
    if (score >= beta) {
      if (!sd->searchInfo.timeout && !sd->extMove) {
        // put the beta cutoff into the TT
        TT.save(key, depth, score_to_tt(score, ply), hd->get_eval_hist(us, ply), m, BOUND_LOWER, ispv);
      }
      // save the cutoff as a killer move since it is likely to be good again
      if (!isCapture && !isPromotion) {
        hd->set_killer(us, m, ply);
      }
      // update the move histories
      mg->update_history(depth + (standpat < alpha), bestScore, beta);
      // return the best score so far
      return bestScore;
    }

    // update alpha if score is beyond it
    if (score > alpha)
      alpha = score;

    // increment the legal moves since the loop has completed
    legalMoves++;

  }

  // if there are no legal moves then it's either stalemate or checkmate
  // we can find out from checking if we are in check or not
  if (legalMoves == 0) {
    if (inCheck) return -VALUE_MATE + ply;
    return VALUE_DRAW;
  }

  // write the current scores to the hash table
  if (!sd->searchInfo.timeout && !sd->extMove) {
    TT.save(key, depth, score_to_tt(bestScore, ply), hd->get_eval_hist(us, ply), bestMove,
            bestMove && ispv ? BOUND_EXACT : BOUND_UPPER, ispv);
  }

  // return the best score
  return bestScore;
}

int Search::qsearch(Position *pos, SearchData *sd, int alpha, int beta, bool ispv) {

  // start with increasing the nodes searched
  sd->searchInfo.nodes++;

  // once again get all the useful info
  History* hd        = &sd->historyData;
  U64      key       = pos->key();
  bool     found     = false;
  int      ply       = pos->get_ply();
  int      bestScore = -VALUE_INFINITE;
  int      standpat;
  int      node      = BOUND_UPPER;
  Move     bestMove  = MOVE_NONE;
  bool     inCheck   = pos->checks();

  // check for a draw here
  if (ply && pos->is_draw()) {
    return 8 - (sd->searchInfo.nodes & 0xF);
  }

  // probe the transposition table for any exisiting entries
  // so we don't have to re-search the same position
  TTentry* tten = TT.get(key, found);
  int ttScore = found ? score_from_tt(tten->score(), ply) : VALUE_NONE;

  // use the score at the given entry
  // only check for a cutoff if the node is not pv
  if (found && ttScore != VALUE_NONE) {
    // set the static eval and the ttmove from the table
    standpat = bestScore = tten->eval();
    // check if tt value is good enough to be returned
    if (!ispv &&
       ((tten->bound() == BOUND_EXACT) ||
       ((tten->bound() == BOUND_LOWER) && (ttScore >= beta)) ||
       ((tten->bound() == BOUND_UPPER) && (ttScore <= alpha)))) {
         return ttScore;
     }
  }
  else {
    standpat = bestScore = inCheck ? -VALUE_MATE + ply : pos->evaluate();
  }

  // use the entry in the TT to adjust the static eval
  if (!inCheck && found) {
    if ((tten->bound() == BOUND_EXACT) ||
       ((tten->bound() == BOUND_LOWER) && (standpat < ttScore)) ||
       ((tten->bound() == BOUND_UPPER) && (standpat > ttScore))) {
      bestScore = ttScore;
    }
  }

  // if standpat is still VALUE_NONE then evaluate a new score
  if (standpat == VALUE_NONE)
    standpat = pos->evaluate();

  if ((bestScore >= beta || ply >= MAX_INTERNAL_PLY) && !inCheck) {
    return bestScore;
  }
  if (alpha < bestScore)
    alpha = bestScore;

  // setup movegen
  MoveGen* mg = &sd->moveGen[ply];

  // initialize the move generator
  Move m;
  mg->init(pos, hd, ply, MOVE_NONE, QSEARCH + inCheck);

  while ((m = mg->next(false))) {
    // check for legality
    if (!pos->is_legal(m))
      continue;

    // static exchange evaluation pruning
    // don't consider a move that is a bad capture at a low depth
    int see = (!inCheck && (pos->is_capture(m) || pos->is_promotion(m))) ? mg->get_see() : 0;
    if (see < 0)
      continue;
    if (see + standpat > beta + 200)
      return beta;

    // do the move
    pos->do_move(m, true);

    bool inCheckOpp = pos->checks();

    // recursively call the qsearch
    int score = -qsearch(pos, sd, -beta, -alpha, ispv);

    // undo the move
    pos->undo_move(m);

    // check for a new best score
    if (score > bestScore) {
      bestScore = score;
      bestMove = m;
      if (score >= beta) {
        node = BOUND_LOWER;
        // store this in the tt table and cut low
        // store with a higher depth dependent on if in check since may help PV search
        TT.save(key, !inCheckOpp, score_to_tt(bestScore, ply), standpat, m, node, ispv);
        return score;
      }
      if (score > alpha) {
        node = BOUND_EXACT;
        alpha = score;
      }
    }
  }
  // check for a checkmate
  if (inCheck && bestScore == -VALUE_INFINITE) return mated_in(ply);

  // store the final position in the hash table
  if (bestMove && node != BOUND_LOWER && !sd->extMove) {
    TT.save(key, 0, score_to_tt(bestScore, ply), standpat, bestMove, node, ispv);
  }
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
  while ((m = mg.next())) {
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
