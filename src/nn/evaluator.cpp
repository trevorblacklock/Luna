#include "evaluator.h"
#include "accumulator.h"
#include "../position.h"

#include <immintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

namespace Luna::NeuralNet {

inline int32_t sum_register_32(vec_reg_32& reg) {
#if defined(__AVX512F__)
  const __m256i reduced8 = _mm256_add_epi16(_mm512_castsi512_si256(reg), _mm512_extracti32x8_epi32(reg, 1));
#elif defined(__AVX2__) || defined(__AVX__)
  const __m256i reduced8 = reg;
#endif

#if defined(__AVX512F__) || defined(__AVX2__) || defined(__AVX__)
  const __m128i reduced4 = _mm_add_epi32(_mm256_castsi256_si128(reduced8), _mm256_extractf128_si256(reduced8, 1));
#else
  const __m128i reduced4 = reg;
#endif

  __m128i vsum = _mm_add_epi32(reduced4, _mm_srli_si128(reduced4, 8));
  vsum =         _mm_add_epi32(vsum, _mm_srli_si128(vsum, 4));
  int32_t sum = _mm_cvtsi128_si32(vsum);
  return sum;
}

Evaluator::Evaluator() {
  this->refreshTable = std::make_unique<RefreshTable>(RefreshTable());
  this->refreshTable->reset();
  this->history.reserve(512);
  this->history.push_back(Accumulator{});
  this->historyIdx = 0;
}

Evaluator::Evaluator(const Evaluator& eval) {
  this->refreshTable->reset();
  this->history = eval.history;
  this->historyIdx = eval.historyIdx;
}

Evaluator& Evaluator::operator=(const Evaluator& eval) {
  this->refreshTable->reset();
  this->history = eval.history;
  this->historyIdx = eval.historyIdx;
  return *this;
}

void Evaluator::reset(Position *pos) {
  this->history.clear();
  this->historyIdx = 0;
  this->history[this->historyIdx].reset(pos);
}

void Evaluator::reset_history() {
  this->history.clear();
  this->history.push_back(Accumulator{});
  this->historyIdx = 0;
}

void Evaluator::update_move_history(Position *pos, Move m, Piece pc, Piece cap, bool undo) {
  const Color side = color_of(pc);
  // if we're undoing a move then increment back
  if (undo) {
    this->history[historyIdx].computed[WHITE] = false;
    this->history[historyIdx].computed[BLACK] = false;
    if (this->historyIdx > 0) {
      this->historyIdx--;
    }
    return;
  }

  // so long as not undo then we can increment forward
  this->historyIdx++;
  // if table is too small then expand it
  if (this->historyIdx >= this->history.size())
    this->history.resize(this->historyIdx + 1);

  for (Color c : {WHITE, BLACK}) {

    // check if the move requires a refresh, else we can lazy update
    if ((side == c) && move_requires_refresh(pc, from_sq(m), to_sq(m))) {
      this->history[this->historyIdx].refresh(pos, this, c);
    }
    else if (this->historyIdx > 0 && this->history[this->historyIdx-1].computed[c]) {
      this->apply_lazy_updates(pos, c, m, pc, cap);
    }
    else {
      // fallback onto a refresh in certain cases
      this->history[this->historyIdx].refresh(pos, this, c);
    }
  }
}

void Evaluator::apply_lazy_updates(Position *pos, Color side, Move m, Piece pc, Piece cap) {
  // get ksq and side
  const Square ksq = pos->get_ksq(side);
  const Color  c   = color_of(pc);

  uint32_t idx = historyIdx;

  uint32_t from = make_index(from_sq(m), pc, ksq, side);
  uint32_t to   = make_index(to_sq(m), pos->is_promotion(m) ? make_piece(c, promotion_type(m)) : pc, ksq, side);

  if (type_of_move(m) == CASTLING) {
    bool ks = KINGSIDE & square_bb(to_sq(m));
    uint32_t rookFrom = make_index(ks ? relative_sq(c, H1) : relative_sq(c, A1), make_piece(c, ROOK), ksq, side);
    uint32_t rookTo   = make_index(ks ? relative_sq(c, F1) : relative_sq(c, D1), make_piece(c, ROOK), ksq, side);
    ssaa_acc(&history[idx-1], &history[idx], side, from, rookFrom, to, rookTo);
  }
  else if (cap != NO_PIECE) {
    Square capSq = (type_of_move(m) == EN_PASSANT) ? to_sq(m) - pawn_push(c) : to_sq(m);
    uint32_t capTo = make_index(capSq, cap, ksq, side);
    ssa_acc(&history[idx-1], &history[idx], side, from, capTo, to);
  }
  else {
    sa_acc(&history[idx-1], &history[idx], side, from, to);
  }

  history[idx].computed[side] = true;

}

int Evaluator::propagate(Color side) {
  constexpr vec_reg_16 reluBias{};

  const auto acc_us   = (vec_reg_16*) &history[historyIdx].values[side];
  const auto acc_them = (vec_reg_16*) &history[historyIdx].values[!side];

  vec_reg_32 res{};
  const auto weight = (vec_reg_16*) hiddenWeights;
  for (int i = 0; i < NB_HIDDEN / INT16_SPACING; i++) {
    res = vec_add_32(res, vec_madd_16(vec_max_16(acc_us[i], reluBias), weight[i]));
  }
  for (int i = 0; i < NB_HIDDEN / INT16_SPACING; i++) {
    res = vec_add_32(res, vec_madd_16(vec_max_16(acc_them[i], reluBias), weight[i + NB_HIDDEN / INT16_SPACING]));
  }
  const auto output = sum_register_32(res) + hiddenBias[0];
  return output / 32 / 128;
}

int Evaluator::predict(Position *pos) {
  history[historyIdx].reset(pos);
  return propagate(pos->get_side());
}
}
