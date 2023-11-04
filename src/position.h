# pragma once

# include "bitboard.h"
# include "nn/evaluator.h"

# include <memory>
# include <deque>
# include <vector>
# include <cstring>
# include <thread>

namespace Luna {

// Position structure that contains game info and hash keys
typedef struct PositionInfo {

  // Keys
  U64 pawnKey;
  U64 key;

  // position variables
  int fifty;
  Piece capturedPiece;
  Square epSquare;
  int castlingRights;
  int pliesFromNull;
  int repetition;

  // curr move
  // used for move histories
  Move move = MOVE_NONE;
  Piece piece = NO_PIECE;

  // check info
  U64 check;
  U64 blockers;
  U64 pinners;
  U64 checkSquares[PIECE_TYPE_NB];

} PositionInfo;

class Position {
private:
  U64 pieces[PIECE_TYPE_NB]; // stores bitboards for each piece black and white
  U64 occupiedSide[COLOR_NB]; // stores occupied boards containing all pieces for each side
  U64 occupied; // stores all pieces on the board regardless on side
  Piece board[SQ_NB]; // stores all pieces per square

  Color side; // side to currently move
  int moveCount; // internal move counter
  int gamePly = 0; // internal ply counter

  std::vector<PositionInfo> PosHistory;



  void update_checks(); // updates position info for checks and pins

public:

  NeuralNet::Evaluator eval;

  // FEN string stuff
  Position(const std::string& fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  Position(const Position& pos);
  Position& operator=(const Position& pos);
  ~Position() = default;
  std::string get_fen() const;

  // Position info
  U64 pc_bb(PieceType pt, Color s) const;
  U64 pt_bb(PieceType pt) const;
  U64 side_bb(Color s) const;
  U64 occ_bb() const;
  U64 non_pawn_mat(Color c) const;
  int get_ply() const;
  void set_ply(int p);
  Piece pc_sq(Square s) const;
  bool is_empty(Square s) const;
  int game_phase();

  // Functions to manipulate bitboards
  void move_piece(Square from, Square to);
  void set_piece(Piece pc, Square s);
  void pop_piece(Square s);

  // Gamestate
  inline PositionInfo* get_pos() { return &PosHistory.back(); }
  inline const PositionInfo* get_pos() const { return &PosHistory.back(); }

  Square ep_square() const;
  int castling_rights() const;
  U64 checks() const;
  U64 blockers() const;
  U64 pinners() const;
  U64 checker_sq(PieceType pt) const;
  int fifty() const;
  int repetition() const;
  Color get_side() const;
  Square get_ksq(Color c) const;
  bool is_on_semiopen_file(Color c, Square s) const;

  // Move related
  Piece captured_piece() const;
  Piece piece_moved(Move m) const;
  bool gives_check(Move m) const;
  bool is_legal(Move m) const;
  bool is_pseudo_legal(Move m) const;
  bool is_capture(Move m) const;
  bool is_promotion(Move m) const;
  void do_move(Move m, bool prefetch = false);
  Move get_previous_move(int ply = 1) const;
  Move get_current_move() const;
  Piece get_previous_piece(int ply = 1) const;
  U64 get_previous_key(int ply = 1) const;

  void undo_move(Move m);
  void do_null_move();
  void undo_null_move();

  // Zobrist keys
  U64 key() const;
  U64 pawn_key() const;
  void update_repetitions();
  bool is_draw() const;
  void change_side();

  // Static exchange evaluation
  int see_eval(Move m) const;

  // evaluation function which calls the network
  int evaluate();

  // Attack related
  U64 attackers(Square s, U64 occ) const;
  U64 attacks_by(Color c, PieceType pt) const;
};

extern std::ostream& operator<<(std::ostream& os, const Position& pos);

inline bool Position::is_on_semiopen_file(Color c, Square s) const {
  return !(pc_bb(PAWN, c) & file_bb(s));
}

inline void Position::set_ply(int p) {
  gamePly = p;
}

inline Move Position::get_current_move() const {
  return get_pos()->move;
}

inline U64 Position::get_previous_key(int ply) const {
  if (PosHistory.size() <= size_t(ply)) return 0;
  return PosHistory[PosHistory.size() - ply].key;
}

inline Move Position::get_previous_move(int ply) const {
  if (PosHistory.size() <= size_t(ply)) return MOVE_NONE;
  return PosHistory[PosHistory.size() - ply].move;
}

inline Piece Position::get_previous_piece(int ply) const {
  if (PosHistory.size() <= size_t(ply)) return NO_PIECE;
  return PosHistory[PosHistory.size() - ply].piece;
}

inline bool Position::is_capture(Move m) const {
  assert(check_move(m));
  if (type_of_move(m) == EN_PASSANT) return true;
  return (pc_sq(to_sq(m)) == NO_PIECE) ? false : true;
}

inline bool Position::is_promotion(Move m) const {
  if (type_of_move(m) == PROMOTION) return true;
  return false;
}

inline bool Position::is_empty(Square s) const {
  return board[s] == NO_PIECE;
}

inline U64 Position::non_pawn_mat(Color c) const {
  return side_bb(c) & (pt_bb(KNIGHT) | pt_bb(BISHOP) | pt_bb(ROOK) | pt_bb(QUEEN));
}

inline void Position::change_side() {
  side = !side;
  get_pos()->key ^= Bitboard::get_hash(SIDE_KEY, SIDE_KEY);
}

inline Square Position::get_ksq(Color c) const {
  return lsb(pc_bb(KING, c));
}

inline Piece Position::captured_piece() const {
  return get_pos()->capturedPiece;
}

inline U64 Position::key() const {
  return get_pos()->key;
}

inline U64 Position::pawn_key() const {
  return get_pos()->pawnKey;
}

inline Square Position::ep_square() const {
  return get_pos()->epSquare;
}

inline int Position::castling_rights() const {
  return get_pos()->castlingRights;
}

inline U64 Position::checks() const {
  return get_pos()->check;
}

inline U64 Position::blockers() const {
  return get_pos()->blockers;
}

inline U64 Position::pinners() const {
  return get_pos()->pinners;
}

inline U64 Position::checker_sq(PieceType pt) const {
  return get_pos()->checkSquares[pt];
}

inline int Position::fifty() const {
  return get_pos()->fifty;
}

inline int Position::repetition() const {
  return get_pos()->repetition;
}

inline Color Position::get_side() const {
  return side;
}

inline int Position::get_ply() const {
  return gamePly;
}

inline Piece Position::pc_sq(Square s) const {
  return board[s];
}

inline Piece Position::piece_moved(Move m) const {
  return pc_sq(from_sq(m));
}

inline U64 Position::pt_bb(PieceType pt) const {
  return pieces[pt];
}

inline U64 Position::pc_bb(PieceType pt, Color c) const {
  return side_bb(c) & pt_bb(pt);
}

inline U64 Position::side_bb(Color c) const {
  return occupiedSide[c];
}

inline U64 Position::occ_bb() const {
  return occupied;
}

inline void Position::set_piece(Piece pc, Square s) {
  board[s] = pc;
  U64 sq = 1ULL << s;
  pieces[piece_type(pc)] |= sq;
  occupied |= sq;
  occupiedSide[color_of(pc)] |= sq;
  get_pos()->key ^= Bitboard::get_hash(pc, s);
  if (piece_type(pc) == PAWN) get_pos()->pawnKey ^= Bitboard::get_hash(pc, s);
}

inline void Position::pop_piece(Square s) {
  Piece pc = pc_sq(s);
  board[s] = NO_PIECE;
  U64 sq = 1ULL << s;
  pieces[piece_type(pc)] ^= sq;
  occupied ^= sq;
  occupiedSide[color_of(pc)] ^= sq;
  get_pos()->key ^= Bitboard::get_hash(pc, s);
  if (piece_type(pc) == PAWN) get_pos()->pawnKey ^= Bitboard::get_hash(pc, s);
}

inline void Position::move_piece(Square from, Square to) {
  Piece pc = pc_sq(from);
  board[from] = NO_PIECE;
  board[to] = pc;
  U64 mask = ((1ULL << from) | (1ULL << to));
  pieces[piece_type(pc)] ^= mask;
  occupied ^= mask;
  occupiedSide[color_of(pc)] ^= mask;
  get_pos()->key ^= (Bitboard::get_hash(pc, from) ^ Bitboard::get_hash(pc, to));
  if (piece_type(pc) == PAWN) get_pos()->pawnKey ^= (Bitboard::get_hash(pc, from) ^ Bitboard::get_hash(pc, to));
}
}
