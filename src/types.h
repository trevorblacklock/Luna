// This is a header file containing all data types used throughout the program
// Contains universal addition of tune.h to optimize eval for elo
// All data types are to be defined in this header

# pragma once

# include <iostream>
# include <cmath>
# include <ctime>
# include <string>
# include <cstdint>
# include <limits.h>

namespace Luna {

typedef uint64_t U64;
typedef uint32_t U32;

typedef uint8_t Square;

typedef uint8_t NodeType;
typedef uint8_t NodeAge;

typedef uint8_t File;
typedef uint8_t Rank;
typedef uint8_t Piece;
typedef uint8_t PieceType;
typedef bool Color;

typedef uint8_t Depth;
typedef uint16_t Score;
typedef uint32_t Evaluation;

typedef uint16_t Move;

constexpr int MAX_PLY = 128;
constexpr int MAX_INTERNAL_PLY = 255;
constexpr int MAX_SEARCH_PLY = 235;
constexpr int MAX_MOVES = 256;

constexpr char STARTPOS[] = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

enum Moves {
  MOVE_NONE,
  MOVE_NULL = 65
};

enum SpecialMoves {
  NORMAL,
  PROMOTION = 1 << 14,
  EN_PASSANT = 2 << 14,
  CASTLING = 3 << 14
};

enum Colors {
  WHITE,
  BLACK,
  COLOR_NB = 2
};

enum CastlingRights {
  NO_CASTLING,
  WK,
  WQ = WK << 1,
  BK = WK << 2,
  BQ = WK << 3
};

enum Directions : int {
  NORTH = 8,
  EAST = 1,
  SOUTH = -NORTH,
  WEST = -EAST,

  NORTH_WEST = NORTH + WEST,
  NORTH_EAST = NORTH + EAST,
  SOUTH_WEST = -NORTH_EAST,
  SOUTH_EAST = -NORTH_WEST
};

enum Files : int {
  FILE_A,
  FILE_B,
  FILE_C,
  FILE_D,
  FILE_E,
  FILE_F,
  FILE_G,
  FILE_H,
  FILE_NB
};

enum Ranks : int {
  RANK_1,
  RANK_2,
  RANK_3,
  RANK_4,
  RANK_5,
  RANK_6,
  RANK_7,
  RANK_8,
  RANK_NB
};

enum Squares {
  A1, B1, C1, D1, E1, F1, G1, H1,
  A2, B2, C2, D2, E2, F2, G2, H2,
  A3, B3, C3, D3, E3, F3, G3, H3,
  A4, B4, C4, D4, E4, F4, G4, H4,
  A5, B5, C5, D5, E5, F5, G5, H5,
  A6, B6, C6, D6, E6, F6, G6, H6,
  A7, B7, C7, D7, E7, F7, G7, H7,
  A8, B8, C8, D8, E8, F8, G8, H8,
  SQ_NONE,
  SQ_NB = 64
};

enum NodeTypes {
  BOUND_NONE,
  BOUND_UPPER,
  BOUND_LOWER,
  BOUND_EXACT = BOUND_UPPER | BOUND_LOWER
};

enum GamePhase {
  PHASE_MIDGAME = 128,
  PHASE_ENDGAME = 0,
  MG = 0, EG = 1,
  PHASE_NB = 2
};

enum PieceTypes {
  PAWN = 1,
  KNIGHT,
  BISHOP,
  ROOK,
  QUEEN,
  KING,
  PIECE_TYPE_NB = 8
};

enum Pieces {
  W_PAWN = 1,
  W_KNIGHT,
  W_BISHOP,
  W_ROOK,
  W_QUEEN,
  W_KING,
  B_PAWN = W_PAWN + 8,
  B_KNIGHT,
  B_BISHOP,
  B_ROOK,
  B_QUEEN,
  B_KING,
  PIECE_NB = 16,
  NO_PIECE = 0
};

enum Value : int {
  VALUE_ZERO = 0,
  VALUE_DRAW = 0,
  VALUE_KNOWN_WIN = 10000,
  VALUE_MATE = 32000,
  VALUE_INFINITE = 32001,
  VALUE_NONE = 32002,

  VALUE_TB_WIN = VALUE_MATE - 2 * MAX_PLY,
  VALUE_TB_LOSS = -VALUE_TB_WIN,
  VALUE_MATE_IN = VALUE_MATE - MAX_PLY,
  VALUE_MATED_IN = -VALUE_MATE_IN,

  PAWN_MG   = 100,  PAWN_EG   = 175,
  KNIGHT_MG = 650,  KNIGHT_EG = 750,
  BISHOP_MG = 700,  BISHOP_EG = 820,
  ROOK_MG   = 1150, ROOK_EG   = 1300,
  QUEEN_MG  = 2300, QUEEN_EG  = 2650,

  MIDGAME_CAP = 13500, ENDGAME_CAP = 3250
};

constexpr int SeeValue[PIECE_TYPE_NB] = {0, 100, 300, 300, 500, 1000, 10000, 0};

constexpr int PieceValue[PHASE_NB][PIECE_NB] = {
  VALUE_ZERO, PAWN_MG, KNIGHT_MG, BISHOP_MG, ROOK_MG, QUEEN_MG, VALUE_ZERO, VALUE_ZERO,
  VALUE_ZERO, PAWN_MG, KNIGHT_MG, BISHOP_MG, ROOK_MG, QUEEN_MG, VALUE_ZERO, VALUE_ZERO,
  VALUE_ZERO, PAWN_EG, KNIGHT_EG, BISHOP_EG, ROOK_MG, QUEEN_EG, VALUE_ZERO, VALUE_ZERO,
  VALUE_ZERO, PAWN_EG, KNIGHT_EG, BISHOP_EG, ROOK_MG, QUEEN_EG, VALUE_ZERO, VALUE_ZERO
};

constexpr char const* SQUARE_ID[SQ_NB] = {
  "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
  "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
  "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5", "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
  "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8"
};

constexpr char const FILE_ID[FILE_NB] = {
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'
};

constexpr char const RANK_ID[RANK_NB] = {
  '1', '2', '3', '4', '5', '6', '7', '8'
};

constexpr char const PIECE_ID[PIECE_NB] = {
  ' ', 'P', 'N', 'B', 'R', 'Q', 'K', ' ',
  ' ', 'p', 'n', 'b', 'r', 'q', 'k', ' '
};

constexpr int const CastlingRights[SQ_NB] = {
  13, 15, 15, 15, 12, 15, 15, 14,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
   7, 15, 15, 15,  3, 15, 15, 11
};

static constexpr Square flip_rank(Square s) {
  return s ^ A8;
}

static constexpr Square flip_file(Square s) {
  return s ^ H1;
}

static constexpr int mated_in(int ply) {
  return -VALUE_MATE + ply;
}

static constexpr int mate_in(int ply) {
  return VALUE_MATE - ply;
}

static constexpr Square rank_file_to_sq(int r, int f) {
  return ((r << 3) + f);
}

static constexpr int make_piece(Color c, Piece pt) {
  return (c << 3) + pt;
}

static constexpr bool check_sq(Square s) {
  return s >= A1 && s <= H8;
}

static constexpr Square from_sq(Move m) {
  return (m >> 6) & 0x3f;
}

static constexpr Square to_sq(Move m) {
  return m & 0x3f;
}

static constexpr PieceType promotion_type(Move m) {
  return ((m >> 12) & 3) + KNIGHT;
}

static constexpr int type_of_move(Move m) {
  return (m & (3 << 14));
}

static constexpr Color color_of(Piece pc) {
  return (pc >> 3);
}

static constexpr PieceType piece_type(Piece pc) {
  return (pc & 7);
}

static constexpr File file_of(Square s) {
  return s & 7;
}

static constexpr Rank rank_of(Square s) {
  return s >> 3;
}

static constexpr int pawn_push(Color c) {
  return c == WHITE ? NORTH : SOUTH;
}

static constexpr bool check_move(Move m) {
  return from_sq(m) != to_sq(m);
}

static constexpr Move make_move(Square from, Square to) {
  return (from << 6) | (to);
}

static constexpr Move make(Square from, Square to, int type, Piece pt = KNIGHT) {
  return Move(type + ((pt - KNIGHT) << 12) + (from << 6) + to);
}

static constexpr Square relative_sq(Color c, Square s) {
  return s ^ (c * 56);
}

static constexpr Rank relative_rank(Color c, Square s) {
  return rank_of(s) ^ (c * 7);
}

constexpr Square make_square(File f, Rank r) {
  return Square((r << 3) + f);
}

constexpr int score_to_tt(int v, int ply) {
  if (v == VALUE_NONE) return VALUE_NONE;
  return v >= VALUE_TB_WIN  ? v + ply
       : v <= VALUE_TB_LOSS ? v - ply : v;
}

constexpr int score_from_tt(int v, int ply) {
  if (v == VALUE_NONE) return VALUE_NONE;
  return v >= VALUE_TB_WIN  ? v - ply
       : v <= VALUE_TB_LOSS ? v + ply : v;
}

constexpr int piece_value(GamePhase phase, Piece pc) {
  return PieceValue[phase][pc];
}

inline std::string square(Square s) {
  return std::string{ char('a' + file_of(s)), char('1' + rank_of(s))};
}

inline std::string move(Move m) {
  std::string move = square(from_sq(m)) + square(to_sq(m));

  if (type_of_move(m) == PROMOTION)
    move += " pnbrqk"[promotion_type(m)];

  return move;
}
}
