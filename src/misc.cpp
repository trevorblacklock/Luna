# include "misc.h"
# include <iterator>

std::vector<std::string> ssplit(const std::string& fen, char delim) {
  std::vector<std::string> res;
  std::stringstream ss (fen);
  std::string i;

  while (getline(ss, i, delim))
    res.push_back(i);

  return res;
}
