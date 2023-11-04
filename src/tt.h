# pragma once

# include "types.h"
# include <memory>

namespace Luna {

struct TTentry {

  Move move() const {return move16;}
  int score() const {return score16;}
  int eval() const {return eval16;}
  int depth() const {return depth16;}
  bool is_pv() const {return (bool)(node8 & 0x4);}
  int bound() const {return node8 & 0x3;}
  int age() const {return age8;}

private:
  friend class TTtable;
  uint32_t key32;
  int16_t depth16;
  int16_t score16;
  int16_t eval16;
  uint16_t move16;
  uint8_t node8;
  uint8_t age8;
};

class TTtable {
private:
  // table configuration
  friend struct TTentry;
  int curAge;
  size_t numEntries;
  size_t sizeEntries;
  TTentry* entries;

public:
  // functions to access the TT
  void save(U64 key, int depth, int score, int eval, Move m, int type, bool pv);
  TTentry* get(U64 key, bool& found) const;
  void clear();
  int tt_size() {return numEntries;};
  // increment age whenever doing a new search
  void new_search() {curAge == 254 ? curAge = 0 : curAge++;}
  // resize TT if needed
  void resize(size_t mb);
  // dealloc the TT on exit for memory leaks
  void dealloc();
  // returns an int 0 - 1000 of how full the hash table is approximately
  int hashfull() const;
  // prefetch used to cache the zobrist key for improved performance
  void prefetch(U64 key) const {__builtin_prefetch(&entries[key & (numEntries - 1)]);}
};

extern TTtable TT;

}
