# include "movegen.h"

namespace Luna {

void MoveGen::init(Position *p) {
  // set position
  psn = p;
  clr = psn->get_side();
  stage = GET_LEGAL;
  mode = PV_SEARCH;
  skip = false;
  captures.size = 0;
  quiets.size = 0;
  searched.size = 0;
  goodCapNum = 0;
  captureIdx = 0;
  quietIdx = 0;
  searchedIdx = 0;
  generate<LEGAL>();
}

void MoveGen::init(Position *p, History *h, int ply, Move hash, int md) {
  // set position
  psn = p;
  hst = h;
  clr = psn->get_side();
  // set movegen conditions
  stage = HASH_MOVE;
  mode = md;
  skip = false;
  captures.size = 0;
  quiets.size = 0;
  searched.size = 0;
  goodCapNum = 0;
  captureIdx = 0;
  quietIdx = 0;
  searchedIdx = 0;
  // set moves
  ttMove = hash;
  killers[0] = h->get_killer(clr, ply, 0);
  killers[1] = h->get_killer(clr, ply, 1);
}

Move MoveGen::next(bool underpromo) {

  switch (stage) {
    case HASH_MOVE:
      stage++;
      if (psn->is_pseudo_legal(ttMove)) return ttMove;
      [[fallthrough]];
    case GEN_CAPTURES:
      generate<CAPTURES>(underpromo);
      stage++;
      [[fallthrough]];
    case GET_GOOD_CAPTURES:
      if (captureIdx < ((mode == QSEARCH_CHECKS) ? captures.size : goodCapNum)) {
        return next_capture();
      }
      if (mode == QSEARCH) return 0;
      if (mode == QSEARCH_CHECKS) {
        stage = GET_EVASIONS;
        memset(killers, 0, sizeof(killers));
        generate<EVASIONS>();
        if (quietIdx < quiets.size) return next_quiet();
        return 0;
      }
      stage++;
      [[fallthrough]];
    case GET_KILLER1:
      stage++;
      if (killers[0] != ttMove && psn->is_pseudo_legal(killers[0])) return killers[0];
      [[fallthrough]];
    case GET_KILLER2:
      stage++;
      if (killers[1] != ttMove && psn->is_pseudo_legal(killers[1])) return killers[1];
      [[fallthrough]];
    case GEN_QUIETS:
      if (skip) {
        stage = GET_BAD_CAPTURES;
        return next();
      }
      generate<QUIETS>();
      stage++;
      [[fallthrough]];
    case GET_QUIETS:
      if (quietIdx < quiets.size) return next_quiet();
      stage++;
      [[fallthrough]];
    case GET_BAD_CAPTURES:
      if (captureIdx < captures.size) return next_capture();
      stage++;
      break;
    case GET_EVASIONS:
      if (quietIdx < quiets.size) return next_quiet();
      break;
    case GET_LEGAL:
      if (captureIdx < captures.size) return next_capture();
      else if (quietIdx < quiets.size) return next_quiet();
      break;
  }
  return 0;
}

void MoveGen::add_move(Move m, int type) {
  switch (type) {
    case CAPTURES:
      // if legal moves then check legality and add
      if (stage == GET_LEGAL) {
        if (psn->is_legal(m)) captures.moves[captures.size++] = m;
        else return;
      }
      // for non legal moves add and compute scores
      else {
        // if hashmove then don't add to list
        if (ttMove == m) return;
        int s = psn->see_eval(m);
        seeScores[captures.size] = s;
        // compute scores with move histories
        // if score is above 0 then it is considered a good capture
        if (s >= 0) {
          s += 100000 + hst->get_history(psn, m);
          goodCapNum++;
        }
        // score is less than 0 so it is a bad capture
        else {
          s = 1000 + hst->get_history(psn, m);
        }
        captures.moves[captures.size] = m;
        captures.scores[captures.size++] = s;
      }
      break;
    case QUIETS:
      // if legal moves then check legality and add
      if (stage == GET_LEGAL) {
        if (psn->is_legal(m)) quiets.moves[quiets.size++] = m;
        else return;
      }
      // for non legal moves add and compute scores
      else {
        // if hashmove or killer move then don't add to list
        if (ttMove == m || killers[0] == m || killers[1] == m) return;
        quiets.moves[quiets.size] = m;
        quiets.scores[quiets.size++] = hst->get_history(psn, m);
      }
      break;
  }
}

void MoveGen::add_searched(Move m) {
  searched.moves[searchedIdx++] = m;
  searched.size++;
}

Move MoveGen::next_capture() {
  // if legal moves then return immediately
  if (stage == GET_LEGAL) return captures.moves[captureIdx++];
  // for non legals then we sort the scores
  else {
    if (skip) return captures.moves[captureIdx++];
    int best = captureIdx;
    // find highest score
    for (int i = captureIdx + 1; i < captures.size; i++) {
      if (captures.scores[i] > captures.scores[best]) best = i;
    }
    // replace best score with current index
    // old index will remain but will never be looked at
    Move m = captures.moves[best];
    currSee = seeScores[best];
    seeScores[best] = seeScores[captureIdx];
    captures.scores[best] = captures.scores[captureIdx];
    captures.moves[best] = captures.moves[captureIdx++];
    return m;
  }
  return 0;
}

Move MoveGen::next_quiet() {
  // if legal moves then return immediately
  if (stage == GET_LEGAL) return quiets.moves[quietIdx++];
  // for non legals then we sort the scores
  else {
    // we only look at moves that give a check
    // discoveries here are not included
    if (skip) {
      for (int i = quietIdx; i < quiets.size; i++) {
        // loop through and only give moves that land on a check sq
        if (square_bb(to_sq(quiets.moves[i])) &
            psn->checker_sq(piece_type(psn->piece_moved(quiets.moves[i])))) {
          quietIdx = i;
          return quiets.moves[quietIdx++];
        }
      }
      // when finish looping we continue through the stages and return the next move
      stage++;
      return next();
    }
    int best = quietIdx;
    // find highest score
    for (int i = quietIdx + 1; i < quiets.size; i++) {
      if (quiets.scores[i] > quiets.scores[best]) best = i;
    }
    // replace best score with current index
    // old index will remain but will never be looked at
    Move m = quiets.moves[best];
    quiets.scores[best] = quiets.scores[quietIdx];
    quiets.moves[best] = quiets.moves[quietIdx++];
    return m;
  }
  return 0;
}

inline void gen_pawn_moves(Position *p, MoveGen *mg, int type, U64 mask, bool underpromo = true) {

  // get side from position
  Color us = p->get_side();
  Color them = !us;

  // move squares
  int from;
  int to;

  // create bitboards and find all pawns that can promote
  U64 ourPawns = p->pc_bb(PAWN, us);
  U64 pawnsOn7 = ourPawns & (us == WHITE ? RANK_7BB : RANK_2BB);
  U64 pawnsNot7 = ourPawns & ~pawnsOn7;
  U64 theirPieces = p->side_bb(them);
  U64 empty = ~p->occ_bb();

  // directions for pawns
  int up = pawn_push(us);
  int upRight = (us == WHITE) ? NORTH_EAST : SOUTH_WEST;
  int upLeft = (us == WHITE) ? NORTH_WEST : SOUTH_EAST;

  U64 b, b1;
  U64 pMask = mask & empty;
  U64 b2 = shift(pawnsNot7, up) & empty;

  if (type == QUIETS) mask &= empty;
  else mask &= theirPieces;

  // generate quiet moves
  if (type == QUIETS) {
    // gen single pawn pushes
    b = b2 & mask;
    // gen double pawn pushes
    b1 = shift((b2 & (us == WHITE ? RANK_3BB : RANK_6BB)), up) & mask;
    while (b) {
      to = pop_lsb(b);
      mg->add_move(make_move(to - up, to), QUIETS);
    }
    while (b1) {
      to = pop_lsb(b1);
      mg->add_move(make_move(to - up - up, to), QUIETS);
    }
  }
  // generate non quiet moves including promotions
  else {
    // gen right side captures
    b = shift(pawnsNot7, upRight) & mask;
    while (b) {
      to = pop_lsb(b);
      mg->add_move(make_move(to - upRight, to), CAPTURES);
    }
    // gen left side captures
    b = shift(pawnsNot7, upLeft) & mask;
    while (b) {
      to = pop_lsb(b);
      mg->add_move(make_move(to - upLeft, to), CAPTURES);
    }
    // gen promotions
    b = shift(pawnsOn7, up) & pMask;
    while (b) {
      to = pop_lsb(b);
      mg->add_move(make(to - up, to, PROMOTION, QUEEN), CAPTURES);
      // only gen underpromotions if told to
      if (underpromo) {
        mg->add_move(make(to - up, to, PROMOTION, ROOK), CAPTURES);
        mg->add_move(make(to - up, to, PROMOTION, KNIGHT), CAPTURES);
        mg->add_move(make(to - up, to, PROMOTION, BISHOP), CAPTURES);
      }
    }
    // gen right side promoting capture
    b = shift(pawnsOn7, upRight) & mask;
    while (b) {
      to = pop_lsb(b);
      mg->add_move(make(to - upRight, to, PROMOTION, QUEEN), CAPTURES);
      // only gen underpromotions if told to
      if (underpromo) {
        mg->add_move(make(to - upRight, to, PROMOTION, ROOK), CAPTURES);
        mg->add_move(make(to - upRight, to, PROMOTION, KNIGHT), CAPTURES);
        mg->add_move(make(to - upRight, to, PROMOTION, BISHOP), CAPTURES);
      }
    }
    // gen left side promoting capture
    b = shift(pawnsOn7, upLeft) & mask;
    while (b) {
      to = pop_lsb(b);
      mg->add_move(make(to - upLeft, to, PROMOTION, QUEEN), CAPTURES);
      // only gen underpromotions if told to
      if (underpromo) {
        mg->add_move(make(to - upLeft, to, PROMOTION, ROOK), CAPTURES);
        mg->add_move(make(to - upLeft, to, PROMOTION, KNIGHT), CAPTURES);
        mg->add_move(make(to - upLeft, to, PROMOTION, BISHOP), CAPTURES);
      }
    }
    // gen en-passant
    if (p->ep_square() != SQ_NONE) {
      b = pawnsNot7 & Attacks::pawn_attacks_bb(them, square_bb(p->ep_square()));
      // while loop since there can be 2 pawns with ability to en-passant
      while (b) {
        from = pop_lsb(b);
        mg->add_move(make(from, p->ep_square(), EN_PASSANT), CAPTURES);
      }
    }
  }
}

inline void gen_piece_moves(Position *p, MoveGen *mg, PieceType pt, int type, U64 mask) {

  // get side
  int us = p->get_side();
  int them = !us;

  Square from;
  Square to;

  // get bitboards to gen moves
  U64 ourPieces = p->pc_bb(pt, us);
  U64 theirPieces = p->side_bb(them);
  U64 empty = ~p->occ_bb();

  if (type == QUIETS) mask &= empty;
  else mask &= theirPieces;

  while (ourPieces) {

    from = pop_lsb(ourPieces);
    U64 attacks = Attacks::attacks_bb(pt, from, p->occ_bb()) & mask;

    while (attacks) {
      to = pop_lsb(attacks);
      mg->add_move(make_move(from, to), type);
    }
  }
}

inline void gen_king_moves(Position *p, MoveGen *mg, int type) {

  // get side
  int us = p->get_side();
  int them = !us;

  Square from = p->get_ksq(us);
  Square to;

  U64 theirPieces = p->side_bb(them);
  U64 empty = ~p->occ_bb();

  U64 mask; // mask for quiet or captures
  if (type == QUIETS || type == EVASIONS) mask = empty;
  else mask = theirPieces;

  U64 attacks = Attacks::attacks_bb(KING, from) & mask;

  int mt = type == EVASIONS ? QUIETS : CAPTURES;

  // add all the basic king moves
  while (attacks) {
    to = pop_lsb(attacks);
    mg->add_move(make_move(from, to), mt);
  }

  // deal with castling
  if (type == QUIETS && !p->checks()) {
    int kingside = (us == WHITE) ? WK : BK;
    int queenside = (us == WHITE) ? WQ : BQ;
    // kingside castle
    if ((p->castling_rights() & kingside) && !(between_bb(from, relative_sq(us, G1)) & p->occ_bb()))
      mg->add_move(make(from, relative_sq(us, G1), CASTLING), QUIETS);
    // queenside castle
    if ((p->castling_rights() & queenside) && !(between_bb(from, relative_sq(us, B1)) & p->occ_bb()))
      mg->add_move(make(from, relative_sq(us, C1), CASTLING), QUIETS);
  }
}

void MoveGen::gen_captures(bool underpromo) {
  // count amount of checking pieces to optimize generation
  int checkNum = popcnt(psn->checks());
  U64 mask = ALL_SQUARES;
  if (checkNum >= 2) {
    gen_king_moves(psn, this, CAPTURES);
    return;
  }
  else if (checkNum == 1) mask = between_bb(psn->get_ksq(psn->get_side()), lsb(psn->checks()));

  gen_pawn_moves(psn, this, CAPTURES, mask, underpromo);
  gen_piece_moves(psn, this, KNIGHT, CAPTURES, mask);
  gen_piece_moves(psn, this, BISHOP, CAPTURES, mask);
  gen_piece_moves(psn, this, ROOK, CAPTURES, mask);
  gen_piece_moves(psn, this, QUEEN, CAPTURES, mask);
  gen_king_moves(psn, this, CAPTURES);

}

void MoveGen::gen_quiets() {
  // count amount of checking pieces to optimize generation
  int checkNum = popcnt(psn->checks());
  U64 mask = ALL_SQUARES;
  if (checkNum >= 2) {
    gen_king_moves(psn, this, QUIETS);
    return;
  }
  else if (checkNum == 1) mask = between_bb(psn->get_ksq(psn->get_side()), lsb(psn->checks()));

  gen_pawn_moves(psn, this, QUIETS, mask);
  gen_piece_moves(psn, this, KNIGHT, QUIETS, mask);
  gen_piece_moves(psn, this, BISHOP, QUIETS, mask);
  gen_piece_moves(psn, this, ROOK, QUIETS, mask);
  gen_piece_moves(psn, this, QUEEN, QUIETS, mask);
  gen_king_moves(psn, this, QUIETS);

}

void MoveGen::gen_evasions() {
  gen_king_moves(psn, this, EVASIONS);
}

template <int type>
void MoveGen::generate(bool underpromo) {
  // switch for different cases
  switch (type) {
    case CAPTURES:
      gen_captures(underpromo);
      break;
    case QUIETS:
      gen_quiets();
      break;
    case EVASIONS:
      gen_evasions();
      break;
    case LEGAL:
      gen_captures();
      gen_quiets();
  }
}
}
