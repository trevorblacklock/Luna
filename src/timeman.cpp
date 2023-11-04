# include "timeman.h"

namespace Luna {

// resets the TimeMan when constructing a new object
TimeMan::TimeMan() {
  reset();
}

// set the depth limit
// the search will be called until this depth has been reached and completed
void TimeMan::set_depth_limit(int depth) {
  depth_limit.enabled = true;
  depth_limit.val = depth;
}

// set the node limit
// the search will be called and stopped once this node count has been exceeded
void TimeMan::set_node_limit(U64 nodes) {
  node_limit.enabled = true;
  node_limit.val = nodes;
}

// set the move time limit
// the search will be given this amount of time in ms to search
void TimeMan::set_move_time_limit(U64 time) {
  move_time_limit.enabled = true;
  move_time_limit.val = time;
}

// set the match time limit
// given the time, increment and expected moves to go it will
// calculate a time to search for, it will also ensure this time
// does not exceed the match limit
void TimeMan::set_match_time_limit(U64 time, U64 inc, int mtg) {
  // setup overhead if there is no increment to account for small delays
  U64 overhead = inc == 0 ? 50 : 0;
  // if low time and no increment then compress the search time further to account for delays
  if (time < 1000 && !inc) time *= 0.7;
  // upper limit to search should not be more than a third of the time left
  U64 upper_bound = time / 3.0;
  // clamp the moves to go to a max of 50
  mtg = mtg > 50 ? 50 : mtg;
  // find the move time
  U64 move_time = 2 * inc + 2 * time / mtg;

  // adjust the move time and upper bound based on game time
  move_time = std::min(time - overhead - inc, move_time);
  upper_bound = std::min(time - overhead - inc, upper_bound);

  // subtract the move overhead from the time
  // make sure to give at least 10ms of time to run a search
  move_time = std::max(static_cast<U64>(10), move_time - move_overhead);
  upper_bound = std::max(static_cast<U64>(10), upper_bound - move_overhead);

  // set the move time limit to the calculated upper bound
  set_move_time_limit(upper_bound);
  // set the match time limits
  match_time_limit.enabled = true;
  match_time_limit.val = move_time;
}

// resets the TimeMan object
void TimeMan::reset() {
  set_start_time();
  stop = false;
  depth_limit = {};
  node_limit = {};
  move_time_limit = {};
  match_time_limit = {};
}

// set the start time
void TimeMan::set_start_time() {
  start_time = std::chrono::duration_cast<std::chrono::milliseconds>(
               std::chrono::steady_clock::now().time_since_epoch()).count();
}

// find the elapsed time using the start time
U64 TimeMan::elapsed_time() const {
  auto now = std::chrono::duration_cast<std::chrono::milliseconds>(
             std::chrono::steady_clock::now().time_since_epoch()).count();
  auto elapsed = now - start_time;
  return elapsed;
}

// set the move overhead
void TimeMan::set_move_overhead(U64 time) {
  move_overhead = time;
}

// forces the search to stop
void TimeMan::stop_search() {
  stop = true;
}

// checks if the search can continue or if the search should stop
bool TimeMan::can_continue() {
  // check if the search is force stopped
  if (stop) return false;
  // get the elapsed time
  U64 elapsed = elapsed_time();
  // check if the set limit has been exceeded
  if (move_time_limit.enabled && move_time_limit.val < elapsed) return false;

  return true;
}

// checks to see if the search can complete another iteration
bool TimeMan::time_to_iterate(int score, int standpat) {
  // check for a force stop
  if (stop) return false;

  // find out how long we have been looking at the best move
  score = 110 - std::min(score, 90);
  // find out the eval for the position
  standpat = std::min(std::max(50, 50 + standpat), 80);
  // get the elapsed time
  U64 elapsed = elapsed_time();

  // check if there is time left using the time limit
  if (move_time_limit.enabled && move_time_limit.val < elapsed) return false;
  // use the calculated scores to find if we should abort the search
  // the scores are used to dictate whether a search would benefit by looking further
  // when a search mainly looks at one best move there likely will not be a better move
  // so we can stop the search earlier and save the time for more important searches
  if (match_time_limit.enabled && match_time_limit.val * score / 100 * standpat / 65 < elapsed) return false;

  // if we haven't dropped out yet then we have time left to iterate
  return true;
}
}
