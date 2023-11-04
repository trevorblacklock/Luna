#include "accumulator.h"
#include "../position.h"

namespace Luna::NeuralNet {

// this is to be used to reset the refresh table to start
// loop through king buckets for each side and set entries blank
void RefreshTable::reset() {
  for (Color c : {WHITE, BLACK}) {
    for (size_t i = 0; i < 32; i++) {
      std::memcpy(this->entries[c][i].values, inputBias, sizeof(int16_t) * NB_HIDDEN);
      std::memset(this->entries[c][i].pieces, 0, sizeof(U64) * PIECE_NB);
    }
  }
}

// this is to reset the accumulator using the pieces on the board
void Accumulator::reset(Position *pos) {
  // loop over both sides
  for (Color c : {WHITE, BLACK}) {
    // get the king square for our side
    const Square ksq = pos->get_ksq(c);
    // reset the state to blank before we add pieces
    int16_t* acc = this->values[c];
    std::memcpy(acc, inputBias, sizeof(int16_t) * NB_HIDDEN);
    // loop through the occupancy board and add to accumulator
    U64 occ = pos->occ_bb();
    while (occ) {
      Square s = pop_lsb(occ);
      Piece p = pos->pc_sq(s);
      apply_delta<true>(acc, make_index(s, p, ksq, c));
    }
    // set the accumulator to be computed
    this->computed[c] = true;
  }
}

void Accumulator::refresh(Position *pos, Evaluator *eval, Color side) {
  // get basic info about pos for entry
  const Square ksq = pos->get_ksq(side);
  const bool   ks  = square_bb(ksq) & KINGSIDE;
  const int    idx = (16 * ks) + KingBuckets[side][ksq];
  // get the refresh entry
  RefreshEntry* state = &eval->refreshTable->entries[side][idx];

  // loop through pieces for each side and update
  for (Color c : {WHITE, BLACK}) {
    for (PieceType pt = PAWN; pt <= KING; pt++) {
      // make the piece
      Piece p = make_piece(c, pt);
      // get the current board from pos
      U64 cur = pos->pc_bb(pt, c);
      U64 pre = state->pieces[p];

      // find which pieces to add and remove
      U64 rem = pre & ~cur;
      U64 add = cur & ~pre;

      // loop through and add/remove all pieces
      while (rem) apply_delta<false>(state->values, make_index(pop_lsb(rem), p, ksq, side));
      while (add) apply_delta<true>(state->values, make_index(pop_lsb(add), p, ksq, side));

      // update the refresh entry with the occupied
      state->pieces[p] = cur;
    }
  }
  // copy over the state and set it computed
  std::memcpy(this->values[side], state->values, sizeof(int16_t) * NB_HIDDEN);
  this->computed[side] = true;
}
}
