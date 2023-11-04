# include "tt.h"
# include <cassert>
# include <cstring>

namespace Luna {

TTtable TT;

void TTtable::save(U64 key, int depth, int score, int eval, Move m, int type, bool pv) {
  // get tt entry using key
  TTentry* tte = &entries[(numEntries - 1) & key];

  // update entry info if empty
  if (tte->key32 == 0) {
    assert(depth >= 0 && depth <= 255);
    tte->key32 = (uint32_t)key;
    tte->move16 = m;
    tte->depth16 = depth;
    tte->score16 = score;
    tte->eval16 = eval;
    tte->node8 = (uint8_t)(pv << 2 | type);
    tte->age8 = curAge;
  }
  // check to see if replacing existing entry is viable
  else {
    if (   (tte->age() != curAge) // if age is different we override
        || (type == BOUND_EXACT) // also always override pv
        || (tte->depth16 <= depth) // override if our depth is equal to or greater
        || (tte->key32 == (uint32_t)key && tte->depth16 <= depth + 3)) { // if depth is greater and key is equal override

      // update the values
      assert(depth >= 0 && depth <= 255);
      tte->key32 = (uint32_t)key;
      tte->move16 = m;
      tte->depth16 = depth;
      tte->score16 = score;
      tte->eval16 = eval;
      tte->node8 = (uint8_t)(pv << 2 | type);
      tte->age8 = curAge;
    }
  }
}

void TTtable::resize(size_t mb) {
  // clear entries if they already exist
  if (entries) free(entries);
  // calculate new size to allocate
  size_t size = mb * 1024  * 1024;
  // setup to allocate it alligned to a power of 2
  constexpr size_t alignment = 2 * 1024 * 1024;
  size_t allocSize = ((size + alignment - 1) / alignment) * alignment;
  entries = static_cast<TTentry*>(std::aligned_alloc(alignment, allocSize));
  // setup size and num entries in class
  sizeEntries = allocSize;
  numEntries = allocSize / sizeof(TTentry);
  curAge = 0;

  clear();
}

void TTtable::clear() {
  curAge = 0;
  if (entries) {
    memset(entries, 0, sizeEntries);
  }
}

void TTtable::dealloc() {
  free(entries);
}


TTentry* TTtable::get(U64 key, bool& found) const {
  // get tt entry using key
  TTentry* tte = &entries[(numEntries - 1) & key];
  if (tte->key32 == 0 || tte->key32 != (uint32_t)key) return found = false, tte;
  else return found = true, tte;
}

int TTtable::hashfull() const {
  int cnt = 0;
  for (int i = 0; i < 1000; i++)
    cnt += (bool)entries[i].key32 && entries[i].age8 == curAge;
  return cnt;
}

}
