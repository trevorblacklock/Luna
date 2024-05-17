# include "position.h"
# include "attacks.h"
# include "tt.h"
# include "nn/accumulator.h"
# include "nn/evaluator.h"

# include <string>
# include <array>
# include <sstream>
# include <iostream>
# include <climits>
# include <iomanip>
# include <algorithm>

namespace Luna {

constexpr Piece Pieces[] = {W_PAWN, W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
                            B_PAWN, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING};

std::ostream& operator<<(std::ostream& os, const Position& pos) {

  os << "   +-----------------+\n";

  // loop through board to find pieces
  for (int r = RANK_8; r >= RANK_1; --r) {
    os << " " << 1 + r << " |";
    for (int f = FILE_A; f <= FILE_H; ++f) {
      Piece pc = pos.pc_sq(make_square(f, r));
      if (pc == NO_PIECE) os << " .";
      else {
        os << " " << PIECE_ID[pc];
      }
    }
    os << " |\n";
  }
  os << "   +-----------------+\n";
  os << "     a b c d e f g h\n"
     << "\nFen: " << pos.get_fen() << "\nKey: 0x" << std::hex << std::uppercase
     << std::setfill('0') << std::setw(16) << pos.key() << std::setfill(' ') << std::dec << "\nCheckers: ";

  for (U64 b = pos.checks(); b;)
    os << square(pop_lsb(b)) << " ";

  return os;
}

// Implementation of Marcel van Kervinck's and Stockfish's cuckoo algorithm
// which detects repetition of positions for 3 fold draws. Uses hash tables
// for the moves and positions to detect a repetition
inline int Hs1(U64 key) { return key & 0x1fff; }
inline int Hs2(U64 key) { return (key >> 16) & 0x1fff; }

std::array<U64, 8192> cuckoo;
std::array<Move, 8192> cuckooMove;

// initialize the cuckoo tables
void Position::init() {
  // ensure tables are filled with known values
  cuckoo.fill(0);
  cuckooMove.fill(MOVE_NONE);

  // loop through and setup tables
  int count = 0;
  for (Piece pc : Pieces)
    for (Square s1 = A1; s1 <= H8; s1++)
      for (Square s2 = s1 + 1; s2 <= H8; s2++)
        if (piece_type(pc) != PAWN && Attacks::attacks_bb(piece_type(pc), s1, 0) & square_bb(s2)) {

          Move m = make_move(s1, s2);
          U64 key = Bitboard::get_hash(pc, s1) ^ Bitboard::get_hash(pc, s2) ^ Bitboard::get_hash(SIDE_KEY, SIDE_KEY);
          int i = Hs1(key);

          while (true) {
            std::swap(cuckoo[i], key);
            std::swap(cuckooMove[i], m);
            if (m == MOVE_NONE)
              break;
            i = (i == Hs1(key)) ? Hs2(key) : Hs1(key);
          }
          count++;
        }
  assert(count == 3668);
}

Position::Position(const std::string& fen) {

  memset(pieces, 0, sizeof(pieces));
  memset(occupiedSide, 0, sizeof(occupiedSide));
  occupied = 0;
  memset(board, NO_PIECE, sizeof(board));

  side = WHITE;

  char token;
  Color c;
  Square sq = A8;
  std::istringstream ff(fen);

  PositionInfo pi {0, 0, 0, NO_PIECE, SQ_NONE, 0, 0, 0, 0, NO_PIECE, 0, 0, 0, {0}};
  this->PosHistory.reserve(512);
  this->PosHistory.push_back(pi);

  ff >> std::noskipws;

  // set pieces on bitboards
  while ((ff >> token) && !isspace(token)) {

    if (isdigit(token))
      sq += (token - '0');

    else if (token == '/')
      sq += 2 * SOUTH;

    else if ((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z')) {

      isupper(token) ? c = WHITE : c = BLACK;

      switch(toupper(token)) {
        case 'P':
          set_piece(make_piece(c, PAWN), sq);
          break;
        case 'N':
          set_piece(make_piece(c, KNIGHT), sq);
          break;
        case 'B':
          set_piece(make_piece(c, BISHOP), sq);
          break;
        case 'R':
          set_piece(make_piece(c, ROOK), sq);
          break;
        case 'Q':
          set_piece(make_piece(c, QUEEN), sq);
          break;
        case 'K':
          set_piece(make_piece(c, KING), sq);
          break;
      }
      sq++;
    }
  }

  // set color to play
  ff >> token;
  if (token == 'b') change_side();
  ff >> token;

  // set castling rights
  while (ff >> token && !isspace(token)) {
    switch(token) {
      case 'K' : get_pos()->castlingRights |= WK; break;
      case 'Q' : get_pos()->castlingRights |= WQ; break;
      case 'k' : get_pos()->castlingRights |= BK; break;
      case 'q' : get_pos()->castlingRights |= BQ; break;
      case '-' : break;
    }
  }

  // set enpassant square
  ff >> token;
  if (token == '-') get_pos()->epSquare = SQ_NONE;
  else {
    int f = token - 'a';
    ff >> token;
    int r = token - '1';
    get_pos()->epSquare = make_square(f, r);
  }

  if (get_pos()->epSquare != SQ_NONE) get_pos()->key ^= Bitboard::get_hash(EP_KEY, get_pos()->epSquare);

  // set fifty move rule and game ply
  ff >> std::skipws >> get_pos()->fifty >> moveCount;

  // setup some NN stuff
  this->eval = NeuralNet::Evaluator();
  this->eval.reset(this);

  this->update_checks();
}

std::string Position::get_fen() const {

  std::string s = "";
  int offset = 0;

  // fill string with pieces
  for (int r = RANK_8; r >= RANK_1; --r) {
    // reset offset
    offset = 0;
    for (int f = FILE_A; f <= FILE_H; ++f) {
      // find square and pc
      int sq = make_square(f, r);
      int pc = pc_sq(sq);
      // handle if no piece and inc offset
      if (pc == NO_PIECE) offset++;
      // handle empty rank
      if (offset >= 8) {
        s += "8";
        offset = 0;
      }
      // handle h-file offsets
      if (f == FILE_H && offset > 0 && pc == NO_PIECE)
        s += std::to_string(offset);
      // handle piece found
      if (pc != NO_PIECE) {
        if (offset > 0) s += std::to_string(offset);
        s += std::string(1, PIECE_ID[pc]);
        offset = 0;
      }
    }
    if (r != RANK_1) s += "/";
  }

  // side to move
  s += (side == WHITE) ? " w " : " b ";

  // castling rights
  if (get_pos()->castlingRights & 0xF) {
    if (get_pos()->castlingRights & WK) s += "K";
    if (get_pos()->castlingRights & WQ) s += "Q";
    if (get_pos()->castlingRights & BK) s += "k";
    if (get_pos()->castlingRights & BQ) s += "q";
  }
  else s += "-";

  // ep square
  if (get_pos()->epSquare != SQ_NONE) {
    s += " " + std::string(SQUARE_ID[get_pos()->epSquare]);
  }
  else s += " -";

  // fifty move and game move rules
  s += " " + std::to_string(get_pos()->fifty) + " " + std::to_string(moveCount);
  return s;
}

Position::Position(const Position& pos) {
  *this = pos;
  this->update_checks();
}

// Copies one Position to another instead of creating an all new one
Position& Position::operator=(const Position& pos) {
  // copy bitboards
  memcpy(pieces, pos.pieces, sizeof(pieces));
  memcpy(occupiedSide, pos.occupiedSide, sizeof(occupiedSide));
  memcpy(board, pos.board, sizeof(board));
  occupied = pos.occupied;

  // copy game infos
  side = pos.side;
  moveCount = pos.moveCount;
  gamePly = pos.gamePly;

  // copy over position history
  PosHistory.clear();
  PosHistory.reserve(512);
  for (int i = 0; i < static_cast<int>(pos.PosHistory.size()); i++) {
    PosHistory.push_back(pos.PosHistory.at(i));
  }

  this->eval.reset(this);

  update_checks();

  return *this;
}

// updates pins and blockers bitboards for legality checks
void Position::update_checks() {

  if (key() == 0) return;

  Color us = side;
  Color them = !side;

  Square ksq = get_ksq(us);
  Square eksq = get_ksq(them);

  // get sliders from opponent bitboards
  U64 latSliders = pc_bb(ROOK, them) | pc_bb(QUEEN, them);
  U64 diagSliders = pc_bb(BISHOP, them) | pc_bb(QUEEN, them);

  U64 sliderAttackers = (Attacks::attacks_bb(ROOK,   ksq, side_bb(them)) & latSliders) |
                        (Attacks::attacks_bb(BISHOP, ksq, side_bb(them)) & diagSliders);

  U64 pinners = 0;
  U64 blockers = 0;
  U64 checkers = (Attacks::pawn_attacks(us, ksq) & pc_bb(PAWN, them)) |
                 (Attacks::attacks_bb(KNIGHT, ksq, 0) & pc_bb(KNIGHT, them));

  while (sliderAttackers) {

    Square sq = pop_lsb(sliderAttackers);
    U64 between = between_bb(ksq, sq) & side_bb(us);

    // count amount of blockers between king and attacking slider
    int blockerNum = popcnt(between);

    if (blockerNum == 0)
      set_bit(checkers, sq);
    else if (blockerNum == 1) {
      set_bit(pinners, sq);
      set_bit(blockers, lsb(between));
    }
  }

  get_pos()->pinners = pinners;
  get_pos()->blockers = blockers;
  get_pos()->check = checkers;
  get_pos()->checkSquares[PAWN] = Attacks::pawn_attacks(them, eksq);
  get_pos()->checkSquares[KNIGHT] = Attacks::attacks_bb(KNIGHT, eksq);
  get_pos()->checkSquares[BISHOP] = Attacks::attacks_bb(BISHOP, eksq, occupied);
  get_pos()->checkSquares[ROOK] = Attacks::attacks_bb(ROOK, eksq, occupied);
  get_pos()->checkSquares[QUEEN] = get_pos()->checkSquares[ROOK] | get_pos()->checkSquares[BISHOP];
}

U64 Position::attackers(Square s, U64 occ) const {

  return (Attacks::pawn_attacks_bb(WHITE, square_bb(s)) & pc_bb(PAWN, BLACK))   |
         (Attacks::pawn_attacks_bb(BLACK, square_bb(s)) & pc_bb(PAWN, WHITE))   |
         (Attacks::attacks_bb(KNIGHT, s) & pt_bb(KNIGHT))                       |
         (Attacks::attacks_bb(BISHOP, s, occ) & (pt_bb(BISHOP) | pt_bb(QUEEN))) |
         (Attacks::attacks_bb(ROOK, s, occ) & (pt_bb(ROOK) | pt_bb(QUEEN)))     |
         (Attacks::attacks_bb(KING, s, occ) & pt_bb(KING));
}

U64 Position::attacks_by(Color c, PieceType pt) const {
  U64 attacks = pc_bb(pt, c);
  if (pt == PAWN)
    return Attacks::pawn_attacks_bb(c, attacks);
  else {
    U64 threat = 0;
    while (attacks) threat |= Attacks::attacks_bb(pt, pop_lsb(attacks), occ_bb());
    return threat;
  }
}

bool Position::is_legal(Move m) const {

  assert(check_move(m));

  Color us = side;
  Color them = !side;
  Square ksq = get_ksq(us);
  Square from = from_sq(m);
  Square to = to_sq(m);

  if (type_of_move(m) == EN_PASSANT) {

    Square capsq = to - pawn_push(us);
    // update occupied board as if capture was made
    U64 occ = (occ_bb() ^ square_bb(from) ^ square_bb(capsq)) | square_bb(to);

    assert(to == ep_square());
    assert(piece_moved(m) == make_piece(us, PAWN));
    assert(pc_sq(capsq) == make_piece(them, PAWN));
    assert(pc_sq(to) == NO_PIECE);

    return !(Attacks::attacks_bb(ROOK, ksq, occ) & (pc_bb(ROOK, them) | pc_bb(QUEEN, them)))
        && !(Attacks::attacks_bb(BISHOP,ksq, occ) & (pc_bb(BISHOP, them) | pc_bb(QUEEN, them)));
  }

  if (type_of_move(m) == CASTLING) {

    int direction = to > from ? WEST : EAST;

    if (checks()) return false;

    for (Square s = to; s != from; s += direction) {
      if (attackers(s, occ_bb()) & side_bb(them))
        return false;
    }
  }

  // if moving piece is king make sure to square is safe
  if (piece_type(pc_sq(from)) == KING)
    return !(attackers(to, occ_bb() ^ square_bb(from)) & side_bb(them));

  // if a different piece is moving then it is legal if is not pinned
  // or if it moves on the same axis as the pin
  // if king is in check then there are no moves for this piece
  if (blockers() & square_bb(from))
    return alligned(from, to, ksq) && !checks();

  // if the king is being attacked by one piece check for blockers
  if (popcnt(checks()) == 1)
    return (between_bb(ksq, lsb(checks())) & square_bb(to));

  // if there are more than 1 attacking piece then the move must be illegal
  // since king moves are already handled
  if (popcnt(checks()) > 1)
    return false;

  return true;

}

void Position::do_move(Move m, bool prefetch) {

  PositionInfo *previous = get_pos();
  PosHistory.emplace_back(PositionInfo {
    previous->pawnKey,
    previous->key,
    previous->fifty,
    NO_PIECE, // reset captured piece
    SQ_NONE, // reset the en-passant square
    previous->castlingRights,
    previous->pliesFromNull,
    1, m, piece_moved(m), 0, 0, 0, {0}});

  // use newly placed position and update as needed
  PositionInfo& newPos = PosHistory.back();

  const Square from = from_sq(m);
  const Square to = to_sq(m);
  const Piece pc = piece_moved(m);
  const int moveType = type_of_move(m);
  const Color us = side;
  const Color them = !side;
  const Piece captured = moveType == EN_PASSANT ? make_piece(them, PAWN) : pc_sq(to);

  assert(pc != NO_PIECE);
  assert(color_of(pc) == us);

  // increment ply counters
  // fifty move rule will be reset if conditions are met
  gamePly++;
  newPos.fifty++;
  newPos.pliesFromNull++;

  // deal with castling moves
  if (moveType == CASTLING) {
    // make sure our king is the piece being moved
    assert(pc == make_piece(us, KING));
    if (relative_sq(us, to) == G1) {
      move_piece(from, to); // move the king
      move_piece(relative_sq(us, H1), relative_sq(us, F1)); // move the rook
    }
    else if (relative_sq(us, to) == C1) {
      move_piece(from, to); // move the king
      move_piece(relative_sq(us, A1), relative_sq(us, D1)); // move the rook
    }
  }

  // deal with captures
  if (captured != NO_PIECE) {

    Square capsq = to;
    if (piece_type(captured) == PAWN) {
      if (moveType == EN_PASSANT) {
        // update capsq
        capsq -= pawn_push(us);
        assert(pc == make_piece(us, PAWN));
        assert(pc_sq(capsq) == make_piece(them, PAWN));
        assert(pc_sq(to) == NO_PIECE);
        assert(to == previous->epSquare);
        assert(relative_rank(us, to) == RANK_6);
      }
    }

    pop_piece(capsq); // remove piece from capsq
    newPos.fifty = 0; // reset fifty move counter
  }

  if (previous->epSquare != SQ_NONE) newPos.key ^= Bitboard::get_hash(EP_KEY, previous->epSquare);

  // update castling rights
  newPos.key ^= Bitboard::get_hash(CASTLE_KEY, newPos.castlingRights);
  newPos.castlingRights &= CastlingRights[from];
  newPos.castlingRights &= CastlingRights[to];
  newPos.key ^= Bitboard::get_hash(CASTLE_KEY, newPos.castlingRights);

  // move the piece
  if (moveType != CASTLING) {
    move_piece(from, to);
  }

  // deal with pawn moves
  if (piece_type(pc) == PAWN) {
    // reset fifty move rule
    newPos.fifty = 0;
    // double pawn pushes
    if (abs(from - to) == 16) {
      newPos.epSquare = from + pawn_push(us);
      newPos.key ^= Bitboard::get_hash(EP_KEY, newPos.epSquare);
    }
    // promotions
    if (moveType == PROMOTION) {
      Piece promoted = make_piece(us, promotion_type(m));
      assert(relative_rank(us, to) == RANK_8);
      assert(piece_type(promoted) >= KNIGHT && piece_type(promoted) <= QUEEN);
      pop_piece(to); // remove pawn from final rank
      set_piece(promoted, to); // add promoted piece instead
    }
  }
  // set captured piece & swap sides
  newPos.capturedPiece = captured;
  change_side();

  if (prefetch) TT.prefetch(newPos.key);

  update_repetitions();
  update_checks();

  eval.update_move_history(this, m, pc, captured, false);
}

void Position::undo_move(Move m) {

  side = !side;
  gamePly--;

  const Square from = from_sq(m);
  const Square to = to_sq(m);
  const int moveType = type_of_move(m);
  const Piece captured = get_pos()->capturedPiece;
  const Color us = side;

  // undo castle move
  if (moveType == CASTLING) {
    if (relative_sq(us, to) == G1) {
      move_piece(to, from); // move the king
      move_piece(relative_sq(us, F1), relative_sq(us, H1)); // move the rook
    }
    else if (relative_sq(us, to) == C1) {
      move_piece(to, from); // move the king
      move_piece(relative_sq(us, D1), relative_sq(us, A1)); // move the rook
    }
  }

  // undo promotion
  else if (moveType == PROMOTION) {
    pop_piece(to);
    set_piece(make_piece(us, PAWN), to);
  }

  // move piece back
  if (moveType != CASTLING) move_piece(to, from);

  // undo en-passant move
  if (captured != NO_PIECE) {
    Square capsq = to;
    if (moveType == EN_PASSANT) {
      capsq -= pawn_push(us);
    }
    set_piece(captured, capsq); // replace captured piece
  }
  eval.update_move_history(this, m, NO_PIECE, NO_PIECE, true);
  PosHistory.pop_back();
}

void Position::do_null_move() {
  PositionInfo *previous = get_pos();
  PosHistory.emplace_back(PositionInfo {
    previous->pawnKey,
    previous->key,
    previous->fifty,
    NO_PIECE, // reset captured piece
    SQ_NONE, // reset the en-passant square
    previous->castlingRights,
    previous->pliesFromNull,
    1, 0, NO_PIECE, 0, 0, 0, {0}});

  // use newly placed position and update as needed
  PositionInfo& newPos = PosHistory.back();

  // update en-passant hash key
  if (previous->epSquare != SQ_NONE) newPos.key ^= Bitboard::get_hash(EP_KEY, previous->epSquare);

  newPos.fifty++;
  newPos.pliesFromNull = 0;
  newPos.repetition = 0;

  change_side();
  update_checks();
}

void Position::undo_null_move() {
  side = !side;
  PosHistory.pop_back();
}

// used to check for possible corruptions in the TT
bool Position::is_pseudo_legal(Move m) const {

  // make sure there is a move before getting any info
  if (!m) return false;

  // get basic information about the move and check for possible corruptions
  const Square from = from_sq(m);
  const Square to = to_sq(m);
  const Piece pc = piece_moved(m);
  const int moveType = type_of_move(m);
  const Color us = side;
  const Color them = !side;
  const Piece captured = moveType == EN_PASSANT ? make_piece(them, PAWN) : pc_sq(to);

  // make sure we are moving our own pieces
  if (color_of(pc) != us) return false;

  // make sure we captured their pieces
  if (captured != NO_PIECE && color_of(captured) != them) return false;

  // make sure we are not capturing king
  if (piece_type(captured) == KING) return false;

  // make sure we aren't moving to the same squares
  if (from == to) return false;

  // make sure if movetype is promotion or en-passant piece is a pawn
  if ((moveType == PROMOTION || moveType == EN_PASSANT) && piece_type(pc) != PAWN) return false;

  // make sure castling move is done by king
  if (moveType == CASTLING && piece_type(pc) != KING) return false;

  // make sure castling rights are available
  if (moveType == CASTLING) {
    // find out if queenside or kingside castling
    if (square_bb(to) & KINGSIDE) {
      int rights = (us == WHITE) ? WK : BK;
      if ((rights & castling_rights()) == 0) return false;
      if ( !(is_empty(relative_sq(us, F1))
          && is_empty(relative_sq(us, G1)))) return false;
    }
    else {
      int rights = (us == WHITE) ? WQ : BQ;
      if ((rights & castling_rights()) == 0) return false;
      if ( !(is_empty(relative_sq(us, D1))
          && is_empty(relative_sq(us, C1))
          && is_empty(relative_sq(us, B1)))) return false;
    }
  }

  // handle pawn moves and make sure squares they move to are empty
  if (piece_type(pc) == PAWN) {
    // only consider pawn moves that move properly
    if (moveType != EN_PASSANT && !(square_bb(to) & Attacks::PSEUDO_PAWN_MOVES[us][from])) return false;

    // for single pawn pushes
    if (abs(from - to) == 8 && !is_empty(to)) return false;

    // for double pushes
    if (abs(from - to) == 16 && (!is_empty(to) || !is_empty(from + pawn_push(us))
        || relative_rank(us, from) != RANK_2)) return false;

    // only allow captures if the pawn moves diagonally
    if (moveType != EN_PASSANT && captured != NO_PIECE
        && !(Attacks::pawn_attacks(us, from) & square_bb(to))) return false;

    // only allow captures so long as a piece is being captured
    if (moveType != EN_PASSANT && (Attacks::pawn_attacks(us, from) & square_bb(to))
        && (pc_sq(to) == NO_PIECE || color_of(pc_sq(to)) == us)) return false;

    // check that en-passant moves are capturing in the correct position
    if (moveType == EN_PASSANT && (pc_sq(to - pawn_push(us)) != make_piece(them, PAWN)
        || relative_rank(us, to) != RANK_6)) return false;

    // check that en-passant moves line up with expected enpassant square
    if (moveType == EN_PASSANT && ep_square() != to) return false;

    // if promotion make sure pawn lands on 1st or 8th rank and starts on 2nd or 7th rank
    if (moveType == PROMOTION
      && !(rank_of(to) == relative_rank(us, A8) && rank_of(from) == relative_rank(us, A7))) return false;
  }

  // check that piece moves properly
  if (piece_type(pc) != PAWN && moveType == NORMAL) {
    if (!(square_bb(to) & Attacks::PSEUDO_ATTACKS[piece_type(pc)][from])) return false;
  }

  // check that there are no pieces between the move unless its a knight
  if (piece_type(pc) != PAWN || piece_type(pc) != KNIGHT || piece_type(pc) != KING) {
    if ((between_bb(from, to) ^ square_bb(to)) & occ_bb()) return false;
  }

  // otherwise the move is true
  return true;
}

bool Position::gives_check(Move m) const {

  Color us = get_side();
  Color them = !us;

  // get move info
  Square from = from_sq(m);
  Square to = to_sq(m);
  int moveType = type_of_move(m);

  // look with check squares to find if there is a direct check
  if (checker_sq(piece_type(pc_sq(from))) & square_bb(to)) return true;

  // look if there is a discovery
  U64 discovery = occ_bb();

  unset_bit(discovery, from);
  set_bit(discovery, to);

  if ((Attacks::attacks_bb(ROOK, get_ksq(them), discovery) & (pc_bb(QUEEN, us) | pc_bb(ROOK, us)))
    | (Attacks::attacks_bb(BISHOP, get_ksq(them), discovery) & (pc_bb(QUEEN, us) | pc_bb(BISHOP, us))))
    return true;

  // look at special cases
  if (moveType == PROMOTION)
    return (Attacks::attacks_bb(promotion_type(m), to, occ_bb() ^ square_bb(from))
            & square_bb(get_ksq(them)));

  if (moveType == EN_PASSANT) {
    Square capsq = to - pawn_push(us);
    U64 occ = (occ_bb() ^ square_bb(from) ^ square_bb(capsq)) | square_bb(to);
    // check for an attack now that we cleared the two pawns and replace the capturing piece
    return (Attacks::attacks_bb(ROOK, get_ksq(them), occ) & (pc_bb(QUEEN, us) | pc_bb(ROOK, us)))
         | (Attacks::attacks_bb(BISHOP, get_ksq(them), occ) & (pc_bb(QUEEN, us) | pc_bb(BISHOP, us)));
  }

  if (moveType == CASTLING) {
    // if we castle kingside then change rook to F1
    Square rookSq = (square_bb(to) & KINGSIDE) ? F1 : D1;
    U64 occ = occ_bb() ^ square_bb(from) ^ square_bb(to);
    // check if rookSq lines up with potential attacks
    return (Attacks::attacks_bb(ROOK, relative_sq(us, rookSq), occ) & square_bb(get_ksq(them)));
  }
  return false;
}

int Position::see_eval(Move m) const {

  // check that move is okay and is not a special case
  assert(check_move(m));
  if (type_of_move(m) != NORMAL) return 0;

  // get move info
  Square from = from_sq(m);
  Square to = to_sq(m);
  PieceType ptFr = piece_type(pc_sq(from));
  PieceType ptTo = piece_type(pc_sq(to));
  Color us = !side;

  // ensure move is a capture
  if (ptTo == NO_PIECE) return 0;

  int score[32];
  int d = 0;
  score[0] = SeeValue[ptTo];

  // get bitboards and remove our piece
  // generate slider attacks since they can pin pieces and have discovered attacks
  U64 occ = occ_bb() ^ square_bb(from) ^ square_bb(to);
  U64 rooks = pt_bb(ROOK);
  U64 bishops = pt_bb(BISHOP);
  U64 queens = pt_bb(QUEEN);

  // make horizontal and diagonal sliders
  U64 horizontal = (rooks | queens) & ~square_bb(from);
  U64 diagonal = (bishops | queens) & ~square_bb(from);

  // find attackers of to square
  U64 attacks = attackers(to, occ);

  // loop through all captures until there are none left
  while (true) {
    // increment the depth
    d++;
    // update attack board and find active attackers
    attacks &= occ;
    U64 activeAttacks = attacks & side_bb(us);
    // if there are no attackers left then we break the loop
    if (!activeAttacks) break;

    // now we find the least valuable piece that can initiate the capture
    PieceType pt;
    for (pt = PAWN; pt <= KING; pt++) {
      if (pc_bb(pt, us) & activeAttacks) break;
    }

    // we can adjust the score based on the move made
    score[d] = SeeValue[ptFr] - score[d - 1];
    // break early if we are double negative
    if (std::max(-score[d - 1], score[d]) < 0) break;
    // update the pt
    ptFr = pt;

    // now we can make the move on our occupied board and then find the from square
    // we also remove that attacker from out board
    Square occFrom = lsb(pc_bb(pt, us) & attacks);
    unset_bit(occ, occFrom);

    // update the attacks board
    if (pt == PAWN || pt == BISHOP || pt == QUEEN)
      attacks |= Attacks::attacks_bb(BISHOP, to, occ) & diagonal;
    if (pt == ROOK || pt == QUEEN)
      attacks |= Attacks::attacks_bb(ROOK, to, occ) & horizontal;

    // change the side
    us = !us;
  }

  // chose best value to represent capture chain
  while (--d) score[d - 1] = -std::max(-score[d - 1], score[d]);

  return score[0];
}

void Position::update_repetitions() {
  int size = PosHistory.size() - 1 - fifty();
  int end = std::max(0, size);
  for (int i = PosHistory.size() - 3; i >= end; i -= 2) {
    if (PosHistory.at(i).key == key())
      get_pos()->repetition = PosHistory.at(i).repetition + 1;
  }
}

bool Position::is_draw() const {
  return fifty() >= 100 || repetition() >= 2;
}

bool Position::has_game_cycle(int ply) const {

  int j;
  int size = PosHistory.size();
  int end = std::min(fifty(), plies_from_null());

  if (end < 3 || size < 3) return false;

  U64 originalKey = key();

  for (int i = 3; i <= end; i += 2) {
    auto state = PosHistory.at(size - i - 1);
    U64 moveKey = originalKey ^ state.key;
    if ((j = Hs1(moveKey), cuckoo[j] == moveKey) || (j = Hs2(moveKey), cuckoo[j] == moveKey)) {
      Move m = cuckooMove[j];
      Square s1 = from_sq(m);
      Square s2 = to_sq(m);

      if (!((between_bb(s1, s2) ^ square_bb(s2)) & occ_bb())) {
        if (ply > i) return true;

        // for nodes before or at the root check that he move is a repetition
        if (color_of(pc_sq(is_empty(s1) ? s2 : s1)) != get_side()) continue;

        // for repetitions before or after root requires one more
        if (state.repetition) return true;
      }
    }
  }
  return false;
}

int Position::game_phase() {
  int npm_w = 0;
  int npm_b = 0;
  for (PieceType pt = KNIGHT; pt < KING; pt++) {
    npm_w += PieceValue[MG][pt] * popcnt(pc_bb(pt, WHITE));
    npm_b += PieceValue[MG][pt] * popcnt(pc_bb(pt, BLACK));
  }
  int npm = std::clamp(npm_w + npm_b, (int)ENDGAME_CAP, (int)MIDGAME_CAP);
  return ((npm - ENDGAME_CAP) * PHASE_MIDGAME) / (MIDGAME_CAP - ENDGAME_CAP);
}

constexpr float phaseValues[PIECE_TYPE_NB] {
    0, 0.552938, 1.55294, 1.50862, 2.64379, 4.0, 0, 0
};

int Position::evaluate() {

  int score = eval.propagate(side);

  constexpr float eval_mg_scale = 1.5;
  constexpr float eval_eg_scale = 1.15;
  constexpr float phase_sum = 39.6684;
  float phase = (phase_sum
                - phaseValues[PAWN] * popcnt(pt_bb(PAWN))
                - phaseValues[KNIGHT] * popcnt(pt_bb(KNIGHT))
                - phaseValues[BISHOP] * popcnt(pt_bb(BISHOP))
                - phaseValues[ROOK] * popcnt(pt_bb(ROOK))
                - phaseValues[QUEEN] * popcnt(pt_bb(QUEEN))) / phase_sum;
  return std::clamp(static_cast<int>((+ eval_mg_scale - phase * (eval_mg_scale - eval_eg_scale)) * score), (int)VALUE_TB_LOSS, (int)VALUE_TB_WIN);
}
}
