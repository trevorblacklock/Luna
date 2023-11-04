# include "attacks.h"
# include "types.h"

namespace Luna {

namespace Attacks {

U64 BishopAttacks[SQ_NB][512];
U64 RookAttacks[SQ_NB][4096];
U64 PAWN_ATTACKS[COLOR_NB][SQ_NB];
U64 PSEUDO_PAWN_MOVES[COLOR_NB][SQ_NB];
U64 PSEUDO_ATTACKS[PIECE_TYPE_NB][SQ_NB];

U64 step(Square s, int step) {
  int to = (s + step);
  return check_sq(to) && SQUARE_DISTANCE[s][to] <= 2 ? square_bb(to) : 0;
}

U64 set_occupancy(int idx, int bim, U64 attack) {
  U64 occupied = 0ULL;

  for (int c = 0; c < bim; c++) {
    int s = pop_lsb(attack);

    if (idx & (1 << c))
      occupied |= (1ULL << s);
  }
  return occupied;
}

U64 sliding_attack(Piece pc, Square s, U64 occupied) {

  U64 attacks = 0ULL;
  int flag;
  flag = (pc == BISHOP);

  int d[2][4] = {NORTH, SOUTH, EAST, WEST, NORTH_EAST, SOUTH_EAST, NORTH_WEST, SOUTH_WEST};

  for (int i = 0; i < 4; i++) {
    int sq = s;
    while (step(sq, d[flag][i]) && !(occupied & square_bb(sq))) {
      attacks |= square_bb(sq += d[flag][i]);
    }
  }
  return attacks;
}

void init_sliders(Piece pc) {
  for (int s = 0; s < 64; s++) {
    U64 attack = (pc == BISHOP) ? BishopMasks[s] : RookMasks[s];
    int rb = popcnt(attack);
    int occupied_idx = (1 << rb);

    for (int i = 0; i < occupied_idx; i++) {
      if (pc == BISHOP) {
        U64 occupied = set_occupancy(i, rb, attack);
        int magic_idx = (occupied * BishopMagics[s]) >> (64 - BishopBits[s]);
        BishopAttacks[s][magic_idx] = sliding_attack(BISHOP, s, occupied);
      }
      else {
        U64 occupied = set_occupancy(i, rb, attack);
        int magic_idx = (occupied * RookMagics[s]) >> (64 - RookBits[s]);
        RookAttacks[s][magic_idx] = sliding_attack(ROOK, s, occupied);
      }
    }
  }
}

void init() {
  int SlidingTypes[2] = {BISHOP, ROOK};
  int KingDirections[8] = {-9, -8, -7, -1, 1, 7, 8, 9};
  int KnightDirections[8] = {-17, -15, -10, -6, 6, 10, 15, 17};

  init_sliders(BISHOP);
  init_sliders(ROOK);

  for (int s1 = A1; s1 <= H8; s1++) {
    PAWN_ATTACKS[WHITE][s1] = pawn_attacks_bb(WHITE, square_bb(s1));
    PAWN_ATTACKS[BLACK][s1] = pawn_attacks_bb(BLACK, square_bb(s1));
    PSEUDO_PAWN_MOVES[WHITE][s1] = PAWN_ATTACKS[WHITE][s1] | square_bb(s1 + NORTH) | square_bb(s1 + NORTH + NORTH);
    PSEUDO_PAWN_MOVES[BLACK][s1] = PAWN_ATTACKS[BLACK][s1] | square_bb(s1 + SOUTH) | square_bb(s1 + SOUTH + SOUTH);

    for (int i = 0; i < 8; i++) {
      PSEUDO_ATTACKS[KING][s1] |= step(s1, KingDirections[i]);
      PSEUDO_ATTACKS[KNIGHT][s1] |= step(s1, KnightDirections[i]);
    }

    PSEUDO_ATTACKS[BISHOP][s1] = attacks_bb(BISHOP, s1);
    PSEUDO_ATTACKS[ROOK][s1] = attacks_bb(ROOK, s1);
    PSEUDO_ATTACKS[QUEEN][s1] = PSEUDO_ATTACKS[BISHOP][s1] | PSEUDO_ATTACKS[ROOK][s1];

    for (int i = 0; i < 2; i++) {
      for (int s2 = A1; s2 <= H8; s2++) {
        if (PSEUDO_ATTACKS[SlidingTypes[i]][s1] & square_bb(s2)) {
          LINE_BB[s1][s2] = (attacks_bb(SlidingTypes[i], s1) & attacks_bb(SlidingTypes[i], s2)) | square_bb(s1) | square_bb(s2);
          BETWEEN_BB[s1][s2] = (attacks_bb(SlidingTypes[i], s1, square_bb(s2)) & attacks_bb(SlidingTypes[i], s2, square_bb(s1)));
        }
        BETWEEN_BB[s1][s2] |= square_bb(s2);
      }
    }
  }
}

}
}
