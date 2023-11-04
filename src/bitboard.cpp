# include "bitboard.h"

namespace Luna {

U64 SQUARE_DISTANCE[SQ_NB][SQ_NB];
U64 SQUARE_BB[SQ_NB];
U64 BETWEEN_BB[SQ_NB][SQ_NB];
U64 LINE_BB[SQ_NB][SQ_NB];

namespace Bitboard {

U64 seed = 668097692;
U64 HASH_KEYS[PIECE_NB][SQ_NB];

U64 random_u64() {
  seed ^= seed << 13;
  seed ^= seed >> 17;
  seed ^= seed << 5;
  return seed;
}

void gen_hash() {
  for (int i = 0; i < PIECE_NB; i++) {
    for (int s = 0; s < SQ_NB; s++) {
      HASH_KEYS[i][s] = random_u64();
    }
  }
}

void init() {

  for (int sq = A1; sq <= H8; sq++) {
    SQUARE_BB[sq] = (1ULL << sq);
  }

  gen_hash();

  for (int s1 = A1; s1 <= H8; s1++) {
    for (int s2 = A1; s2 <= H8; s2++) {
      SQUARE_DISTANCE[s1][s2] = std::max(distance_f(s1, s2), distance_r(s1, s2));
    }
  }
}

void print_bitboard(U64 b) {
  std::string s = "   +-----------------+\n";

  for (int r = RANK_8; r >= RANK_1; --r) {
    s += " " + std::to_string(1 + r) + " |";
    for (int f = FILE_A; f <= FILE_H; ++f)
      s += b & square_bb(make_square(f, r)) ? " x" : " .";
    s += " |\n";
  }
  s += "   +-----------------+\n";
  s += "     a b c d e f g h\n";
  printf("%s", s.c_str());
}

}

}
