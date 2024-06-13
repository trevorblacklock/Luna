# pragma once

# include "types.h"

# include <cassert>

namespace Luna {

constexpr U64 ZERO = 0ULL;
constexpr U64 ONE = 1ULL;

constexpr U64 ALL_SQUARES = ~ZERO;
constexpr U64 DARK_SQUARES = 0xAA55AA55AA55AA55ULL;


constexpr U64 FILE_ABB = 0x0101010101010101ULL;
constexpr U64 FILE_BBB = FILE_ABB << 1;
constexpr U64 FILE_CBB = FILE_ABB << 2;
constexpr U64 FILE_DBB = FILE_ABB << 3;
constexpr U64 FILE_EBB = FILE_ABB << 4;
constexpr U64 FILE_FBB = FILE_ABB << 5;
constexpr U64 FILE_GBB = FILE_ABB << 6;
constexpr U64 FILE_HBB = FILE_ABB << 7;

constexpr U64 RANK_1BB = 0xFF;
constexpr U64 RANK_2BB = RANK_1BB << (8 * 1);
constexpr U64 RANK_3BB = RANK_1BB << (8 * 2);
constexpr U64 RANK_4BB = RANK_1BB << (8 * 3);
constexpr U64 RANK_5BB = RANK_1BB << (8 * 4);
constexpr U64 RANK_6BB = RANK_1BB << (8 * 5);
constexpr U64 RANK_7BB = RANK_1BB << (8 * 6);
constexpr U64 RANK_8BB = RANK_1BB << (8 * 7);

constexpr U64 NOT_FILE_ABB = ~FILE_ABB;
constexpr U64 NOT_FILE_HBB = ~FILE_HBB;
constexpr U64 NOT_RANK_1BB = ~RANK_1BB;
constexpr U64 NOT_RANK_8BB = ~RANK_8BB;

constexpr U64 OUTER_SQUARES_BB = 0xFF818181818181FFULL;

constexpr U64 QUEENSIDE = FILE_ABB | FILE_BBB | FILE_CBB | FILE_DBB;
constexpr U64 KINGSIDE = FILE_EBB | FILE_FBB | FILE_GBB | FILE_HBB;
constexpr U64 CENTER_FILES = FILE_CBB | FILE_DBB | FILE_EBB | FILE_FBB;
constexpr U64 CENTER = (FILE_DBB | FILE_EBB) & (RANK_4BB | RANK_5BB);

constexpr U64 KINGFLANK[FILE_NB] = {
  QUEENSIDE ^ FILE_DBB, QUEENSIDE, QUEENSIDE,
  CENTER_FILES, CENTER_FILES, KINGSIDE,
  KINGSIDE, KINGSIDE ^ FILE_EBB
};

extern U64 seed;
constexpr int CastlingMask[64] = {
	13, 15, 15, 15, 12, 15, 15, 14,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15,
   7, 15, 15, 15,  3, 15, 15, 11
};

enum Hash {
  SIDE_KEY,
  CASTLE_KEY = 7,
  EP_KEY = 8
};

extern U64 SQUARE_DISTANCE[SQ_NB][SQ_NB];
extern U64 SQUARE_BB[SQ_NB];
extern U64 BETWEEN_BB[SQ_NB][SQ_NB];
extern U64 LINE_BB[SQ_NB][SQ_NB];

// shift moves a bitboard in direction d by one step
constexpr U64 shift(U64 b, int d) {
  return d == NORTH       ? b << 8               : d == SOUTH       ? b >> 8
       : d == NORTH+NORTH ? b << 16              : d == SOUTH+SOUTH ? b >> 16
       : d == EAST        ? (b & ~FILE_HBB) << 1 : d == WEST        ? (b & ~FILE_ABB) >> 1
       : d == NORTH_EAST  ? (b & ~FILE_HBB) << 9 : d == NORTH_WEST  ? (b & ~FILE_ABB) << 7
       : d == SOUTH_EAST  ? (b & ~FILE_HBB) >> 7 : d == SOUTH_WEST  ? (b & ~FILE_ABB) >> 9
       : 0;
}

constexpr U64 rank_bb(Square s) {
  return RANK_1BB << (8 * rank_of(s));
}

constexpr U64 file_bb(Square s) {
  return FILE_ABB << file_of(s);
}

inline U64 line_bb(Square s1, Square s2) {
  return LINE_BB[s1][s2];
}

inline U64 between_bb(Square s1, Square s2) {
  return BETWEEN_BB[s1][s2];
}

inline U64 square_bb(Square s) {
  return SQUARE_BB[s];
}

constexpr U64 adjacent_files_bb(Square s) {
  return (shift(file_bb(s), EAST) | shift(file_bb(s), WEST));
}

constexpr U64 forward_ranks_bb(Color c, Square s) {
  return c == WHITE ? ~RANK_1BB << 8 * relative_rank(WHITE, s)
                    : ~RANK_8BB >> 8 * relative_rank(BLACK, s);
}

constexpr U64 forward_file_bb(Color c, Square s) {
  return forward_ranks_bb(c, s) & file_bb(s);
}

constexpr U64 pawn_adjacent_attacks(Color c, Square s) {
  return forward_ranks_bb(c, s) & adjacent_files_bb(s);
}

constexpr U64 passed_pawn_mask(Color c, Square s) {
  return pawn_adjacent_attacks(c, s) | forward_file_bb(c, s);
}

inline bool alligned(Square s1, Square s2, Square s3) {
  return line_bb(s1, s2) & square_bb(s3);
}

inline int edge_distance_f(File f) {
  return std::min(f, File(FILE_H - f));
}

inline int edge_distance_r(Rank r) {
  return std::min(r, Rank(RANK_8 - r));
}

constexpr bool more_than_one(U64 b) {
  return b & (b - 1);
}

inline void toggle_bit(U64& n, Square s) {
  n ^= (1ULL << s);
}

inline void set_bit(U64& n, Square s) {
  n |= (1ULL << s);
}

inline void unset_bit(U64& n, Square s) {
  n &= ~(1ULL << s);
}

inline bool get_bit(U64& n, Square s) {
  return (n & (1ULL << s));
}

inline int popcnt(U64 b) {
  #if defined(_WIN32) || defined(WIN32)
  return int(_mm_popcnt_u64(b));
  #else
  return __builtin_popcountll(b);
  #endif
}

inline Square lsb(U64 b) {
  assert(b);
  #if defined(_WIN32) || defined(WIN32)
  unsigned long idx;
  _BitScanForward64(&idx, b);
  return Square(idx);
  #else
  return Square(__builtin_ctzll(b));
  #endif
}

inline Square msb(U64 b) {
  assert(b);
  #if defined(_WIN32) || defined(WIN32)
  unsigned long idx;
  _BitScanReverse64(&idx, b);
  return Square(idx);
  #else
  return Square(__builtin_clzll(b));
  #endif
}

inline U64 lsb_bb(U64 b) {
  assert(b);
  return b & -b;
}

inline Square pop_lsb(U64& b) {
  assert(b);
  const Square s = lsb(b);
  b &= b - 1;
  return s;
}

inline Square frontmost_sq(Color c, U64 b) {
  assert(b);
  return c == WHITE ? msb(b) : lsb(b);
}

inline int distance_f(Square x, Square y) {return std::abs(file_of(x) - file_of(y));}
inline int distance_r(Square x, Square y) {return std::abs(rank_of(x) - rank_of(y));}
inline int distance(Square x, Square y) {return SQUARE_DISTANCE[x][y];}

namespace Bitboard {
  void print_bitboard(U64 b);
  void init();

  extern U64 HASH_KEYS[PIECE_NB][SQ_NB];

  inline U64 get_hash(Piece pc, Square sq) {return HASH_KEYS[pc][sq];}
}
}
