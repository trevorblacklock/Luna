# include <cstring>
# include "pv.h"

namespace Luna {

// base operators for pv struct
Move& PvLine::operator()(int depth) {return pv[depth];}
Move PvLine::operator()(int depth) const {return pv[depth];}
PvLine& PvTable::operator()(int depth) {return pvs[depth];}
PvLine PvTable::operator()(int depth) const {return pvs[depth];}
// pv functions used to clear and update the pv
void PvTable::reset() {
  for (auto &r : pvs) {
    r.length = 0;
  }
}
void PvTable::update(int ply, Move m) {
  pvs[ply](0) = m;
  memcpy(&pvs[ply](1), &pvs[ply + 1](0), sizeof(Move) * pvs[ply + 1].length);
  pvs[ply].length = pvs[ply + 1].length + 1;
}
}
