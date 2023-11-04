#pragma once

#include "../bitboard.h"
#include "common.h"
#include "layers.h"

namespace Luna::NeuralNet {

struct RefreshEntry {
  alignas(BYTE_ALIGNMENT) int16_t values[NB_HIDDEN] {};
  U64 pieces[PIECE_NB];
};

struct RefreshTable {
  RefreshEntry entries[COLOR_NB][32] {};
  void reset();
};

struct Accumulator {
  bool computed[COLOR_NB] = {false, false};
  alignas(BYTE_ALIGNMENT) int16_t values[COLOR_NB][NB_HIDDEN] {};
  void reset(Position *pos);
  void refresh(Position *pos, Evaluator *eval, Color side);
};

static constexpr int KingBuckets[COLOR_NB][SQ_NB] {
  { 0,  1,  2,  3,  3,  2,  1,  0,
    4,  5,  6,  7,  7,  6,  5,  4,
    8,  9,  10, 11, 11, 10, 9,  8,
    8,  9,  10, 11, 11, 10, 9,  8,
    12, 12, 13, 13, 13, 13, 12, 12,
    12, 12, 13, 13, 13, 13, 12, 12,
    14, 14, 15, 15, 15, 15, 14, 14,
    14, 14, 15, 15, 15, 15, 14, 14 },
  { 14, 14, 15, 15, 15, 15, 14, 14,
    14, 14, 15, 15, 15, 15, 14, 14,
    12, 12, 13, 13, 13, 13, 12, 12,
    12, 12, 13, 13, 13, 13, 12, 12,
    8,  9,  10, 11, 11, 10, 9,  8,
    8,  9,  10, 11, 11, 10, 9,  8,
    4,  5,  6,  7,  7,  6,  5,  4,
    0,  1,  2,  3,  3,  2,  1,  0 }
};

static constexpr int OrientSq[COLOR_NB][SQ_NB] = {
  { A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8 },
  { A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1 }
};

inline bool move_requires_refresh(Piece pc, Square from, Square to) {
  if (piece_type(pc) != KING) return false;
  if (file_of(from) + file_of(to) == 7) return true;
  return KingBuckets[color_of(pc)][from] != KingBuckets[color_of(pc)][to];
}

inline uint32_t make_index(Square sq, Piece pc, Square ksq, Color side) {

  sq = OrientSq[side][sq];
  sq ^= 7 * !!(ksq & 0x4);

  uint32_t idx =  sq
               + (piece_type(pc) - 1) * 64
               + !(color_of(pc) ^ side) * 64 * 6
               + KingBuckets[side][ksq] * 64 * 6 * 2;

  assert(idx < NB_FEATURES);
  return idx;
}

template<bool add>
inline void apply_delta(int16_t* source, int16_t* target, const uint32_t idx) {

  vec_reg_16 regs[NB_REGISTER];

  for (size_t i = 0; i < NB_HIDDEN / CHUNK_UNROLL; i++) {
    const size_t unrollOffset = i * CHUNK_UNROLL;

    const auto weight = (vec_reg_16*) &inputWeights[idx * NB_HIDDEN + unrollOffset];
    const auto input  = (vec_reg_16*) &source[unrollOffset];
    auto       output = (vec_reg_16*) &target[unrollOffset];

    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_load(&input[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = add ? vec_add_16(regs[x], weight[x])
                    : vec_sub_16(regs[x], weight[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      vec_store(&output[x], regs[x]);
  }
}

template<bool add>
inline void apply_delta(int16_t* acc, const uint32_t idx) {
  apply_delta<add>(acc, acc, idx);
}

inline void sa_acc(Accumulator* source, Accumulator* target, Color side,
                   uint32_t i1, uint32_t i2) {

  vec_reg_16 regs[NB_REGISTER];

  const auto in  = source->values[side];
  const auto out = target->values[side];

  for (int i = 0; i < NB_HIDDEN / CHUNK_UNROLL; i++) {
    const size_t unrollOffset = i * CHUNK_UNROLL;

    auto w1     = (vec_reg_16*) &inputWeights[i1 * NB_HIDDEN + unrollOffset];
    auto w2     = (vec_reg_16*) &inputWeights[i2 * NB_HIDDEN + unrollOffset];
    auto input  = (vec_reg_16*) &in[unrollOffset];
    auto output = (vec_reg_16*) &out[unrollOffset];

    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_load(&input[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_sub_16(regs[x], w1[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_add_16(regs[x], w2[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      vec_store(&output[x], regs[x]);
  }
}

inline void ssa_acc(Accumulator* source, Accumulator* target, Color side,
                    uint32_t i1, uint32_t i2, uint32_t i3) {

  vec_reg_16 regs[NB_REGISTER];

  const auto in  = source->values[side];
  const auto out = target->values[side];

  for (int i = 0; i < NB_HIDDEN / CHUNK_UNROLL; i++) {
    const size_t unrollOffset = i * CHUNK_UNROLL;

    auto w1     = (vec_reg_16*) &inputWeights[i1 * NB_HIDDEN + unrollOffset];
    auto w2     = (vec_reg_16*) &inputWeights[i2 * NB_HIDDEN + unrollOffset];
    auto w3     = (vec_reg_16*) &inputWeights[i3 * NB_HIDDEN + unrollOffset];
    auto input  = (vec_reg_16*) &in[unrollOffset];
    auto output = (vec_reg_16*) &out[unrollOffset];

    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_load(&input[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_sub_16(regs[x], w1[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_sub_16(regs[x], w2[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_add_16(regs[x], w3[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      vec_store(&output[x], regs[x]);
  }
}

inline void ssaa_acc(Accumulator* source, Accumulator* target, Color side,
                     uint32_t i1, uint32_t i2, uint32_t i3, uint32_t i4) {

  vec_reg_16 regs[NB_REGISTER];

  const auto in  = source->values[side];
  const auto out = target->values[side];

  for (int i = 0; i < NB_HIDDEN / CHUNK_UNROLL; i++) {
    const size_t unrollOffset = i * CHUNK_UNROLL;

    auto w1     = (vec_reg_16*) &inputWeights[i1 * NB_HIDDEN + unrollOffset];
    auto w2     = (vec_reg_16*) &inputWeights[i2 * NB_HIDDEN + unrollOffset];
    auto w3     = (vec_reg_16*) &inputWeights[i3 * NB_HIDDEN + unrollOffset];
    auto w4     = (vec_reg_16*) &inputWeights[i4 * NB_HIDDEN + unrollOffset];
    auto input  = (vec_reg_16*) &in[unrollOffset];
    auto output = (vec_reg_16*) &out[unrollOffset];

    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_load(&input[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_sub_16(regs[x], w1[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_sub_16(regs[x], w2[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_add_16(regs[x], w3[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      regs[x] = vec_add_16(regs[x], w4[x]);
    for (size_t x = 0; x < NB_REGISTER; x++)
      vec_store(&output[x], regs[x]);
  }
}

}
