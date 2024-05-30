# include "uci.h"

# include "types.h"
# include "bitboard.h"
# include "attacks.h"
# include "position.h"
# include "search.h"
# include "misc.h"
# include "timer.h"
# include "timeman.h"
# include "tt.h"

# include <stdio.h>
# include <cstdlib>
# include <iostream>
# include <string>
# include <thread>
# include <vector>

namespace Luna {

// create objects used by the search
TimeMan timeMan{};
Search sh{};
std::thread mainThread;

// returns a move from string format
// also checks legality of move
inline Move move_from(Position *p, std::string m) {
  // check for a promotion and ensure lowercase
  if (m.length() == 5)
    m[4] = char(tolower(m[4]));
  // create new movegen object to generate all moves
  MoveGen mg;
  Move mv;
  mg.init(p);
  // loop through the moves to find if it exists
  while ((mv = mg.next())) {
    if (m == move(mv))
      return mv;
  }
  return MOVE_NONE;
}

// given a subset of the string finds the string next over seperated by a space
// for example given "hello world" and the token "hello", will return "world"
// this is useful for parsing uci commands that typically have a keyword followed by a value
inline std::string find_val_from_keyword(std::string whole, std::string key) {
  std::vector<std::string> str = ssplit(whole, ' ');
  for (int i = 0; i < (int)str.size(); i++) {
    if (str.at(i) == key) return str.at(i + 1);
  }
  return "";
}

// run the search given the time managers options then print the bestmove
void ucisearch(TimeMan *tm, Position *pos) {
  Move m = sh.search(pos, tm);
  std::cout << "bestmove " << move(m) << std::endl;
}

void uci::loop(int argc, char* argv[]) {
  // initialize everything
  Bitboard::init();
  Attacks::init();
  Position::init();
  NeuralNet::init();
  init_lmr();

  // setup objects
  Position pos;
  sh = Search();

  // setup TT
  TT.resize(16);

  // setup timeman and set default threads
  timeMan = TimeMan();
  sh.set_threads(1);

  // send engine info to the gui
  std::cout << "Luna " << MAJOR_VERSION
            << "." << MINOR_VERSION << " by T. Blacklock"
            << std::endl;

  // run proper quit function upon exit to clean up
  std::atexit(uci::quit);

  // process commands from shell
  std::string l;
  for (int i = 1; i < argc; i++) {
    uci::parse(argv[i], pos);
    if (strcmp(argv[i], "bench") == 0) {
      parse("exit", pos);
    }
  }
  // process commands sent from gui
  while (std::getline(std::cin, l)) {
    uci::parse(l, pos);
  }
}

void uci::uci() {
  std::cout << "id name Luna " << MAJOR_VERSION << "." << MINOR_VERSION << std::endl;
  std::cout << "id author T. Blacklock" << std::endl;
  std::cout << "option name Hash type spin default 16 min 1 max " << ((1 << 12) * sizeof(TTentry)) << std::endl;
  std::cout << "option name Threads type spin default 1 min 1 max " << std::thread::hardware_concurrency() << std::endl;
  std::cout << "option name MoveOverhead type spin default 0 min 0 max 1000" << std::endl;
  std::cout << "uciok" << std::endl;
}

void uci::parse(std::string args, Position& pos) {
  // split the input using spaces
  std::vector<std::string> str = ssplit(args, ' ');
  // clear hash table and heuristics
  if (str.at(0) == "ucinewgame") {
    TT.clear();
    sh.clear_hist();
  }
  // print uci info
  if (str.at(0) == "uci")
    uci::uci();
  // setoption parse
  else if (str.at(0) == "setoption") {
    // find option and val using keywords
    std::string option = find_val_from_keyword(args, "name");
    std::string val = find_val_from_keyword(args, "value");
    // run the command
    uci::set_option(option, val);
  }
  else if (str.at(0) == "go") {
    uci::parse_go(args, pos);
  }
  else if (str.at(0) == "position") {
    uci::parse_position(args, pos);
  }
  else if (str.at(0) == "isready") {
    std::cout << "readyok" << std::endl;
  }
  else if (str.at(0) == "stop") {
    uci::stop();
  }
  else if (str.at(0) == "d") {
    std::cout << pos << std::endl;
  }
  else if (str.at(0) == "eval") {
    NeuralNet::Evaluator ev;
    std::cout << ev.predict(&pos) << std::endl;
  }
  else if (str.at(0) == "bench") {
    bench();
  }
  else if (str.at(0) == "exit" || str.at(0) == "quit") {
    exit(0);
  }
}

void uci::bench() {
  // positions from Koivisto
  static const char* Benchmarks[] = {
#include "bench.csv"
  ""};

  int nodes = 0;
  int time = 0;

  // disable info strings for bench
  sh.disable_info_strings();
  for (int i = 0; strcmp(Benchmarks[i], ""); i++) {
    // initialize a new position with the fen
    Position bch(Benchmarks[i]);

    // initialize a time manager, and search the move
    TimeMan manager{};
    manager.set_depth_limit(12);
    Move best = sh.search(&bch, &manager);

    // grab info from search
    nodes += sh.searchInfo.nodes;
    time += sh.searchInfo.elapsed;

    printf("Bench [# %2d] %5d cp  Best:%6s  %12d nodes %8d nps", i + 1,
          sh.searchInfo.score, move(best).c_str(), static_cast<int>(sh.searchInfo.nodes),
          static_cast<int>(1000.0f * sh.searchInfo.nodes / (sh.searchInfo.elapsed + 1)));
    std::cout << std::endl;

    // clear up the search object
    sh.clear_hist();
    TT.clear();
  }
  printf("OVERALL: %39d nodes %8d nps", static_cast<int>(nodes), static_cast<int>(1000.0f * nodes / (time + 1)));
  std::cout << std::endl;
  sh.enable_info_strings();
}

void uci::parse_go(std::string args, Position& pos) {
  // stop any search already going
  uci::stop();

  // first thing to do is check if the arg is for perft
  if (args.find("perft") != std::string::npos) {
    // find the depth and run perft
    std::string str = find_val_from_keyword(args, "perft");
    int depth = stoi(str);
    // call perft and setup return to print to terminal
    U64 nodes = sh.perft(&pos, depth);
    std::cout << "Nodes Searched: " << nodes << std::endl;
    // return once done perft call
    return;
  }

  timeMan.reset();

  if (args.find("wtime") != std::string::npos || args.find("btime") != std::string::npos ||
      args.find("winc")  != std::string::npos || args.find("binc")  != std::string::npos ||
      args.find("movestogo") != std::string::npos) {

    // set all the times and give default values if strings are empty
    std::string wtime_str = find_val_from_keyword(args, "wtime");
    std::string btime_str = find_val_from_keyword(args, "btime");
    std::string winc_str = find_val_from_keyword(args, "winc");
    std::string binc_str = find_val_from_keyword(args, "binc");
    std::string mtg_str = find_val_from_keyword(args, "movestogo");

    U64 wtime = wtime_str.empty() ? 0  : std::stoi(wtime_str);
    U64 btime = btime_str.empty() ? 0  : std::stoi(btime_str);
    U64 winc = winc_str.empty()   ? 0  : std::stoi(winc_str);
    U64 binc = binc_str.empty()   ? 0  : std::stoi(binc_str);
    int mtg = mtg_str.empty()     ? 40 : std::stoi(mtg_str);

    // set the values into the time manager
    timeMan.set_match_time_limit(pos.get_side() == WHITE ? wtime : btime,
                                 pos.get_side() == WHITE ? winc  : binc, mtg);
  }
  if (args.find("depth") != std::string::npos) {
    timeMan.set_depth_limit(stoi(find_val_from_keyword(args, "depth")));
  }
  if (args.find("nodes") != std::string::npos) {
    timeMan.set_node_limit(stoull(find_val_from_keyword(args, "nodes")));
  }
  if (args.find("movetime") != std::string::npos) {
    timeMan.set_move_time_limit(stoull(find_val_from_keyword(args, "movetime")));
  }

  // now start the search given the time manager
  mainThread = std::thread(ucisearch, &timeMan, &pos);
}

void uci::set_option(std::string option, std::string val) {
  // set the hash table size
  if (option == "Hash") {
    TT.resize(stoi(val));
  }
  else if (option == "Threads") {
    sh.set_threads(stoi(val));
  }
  else if (option == "MoveOverhead") {
    timeMan.set_move_overhead(stoi(val));
  }
}

void uci::parse_position(std::string args, Position& pos) {
  // find the fen and the moves
  auto pos1 = args.find("fen");
  auto pos2 = args.find("moves");

  std::string moves;
  if (pos2 != std::string::npos) {
    // extract moves and use them after fen is parsed
    moves = args.substr(pos2 + 6);
  }
  if (pos1 != std::string::npos) {
    // extract fen and setup new position with it
    std::string fen = args.substr(pos1 + 4);
    pos = Position(fen);
  }
  else {
    // set to default starting position
    pos = Position();
  }

  // prepare loop to do moves and to check legality of moves
  if (moves.empty()) return;
  // split the moves into an array storing all of them
  std::vector<std::string> allMoves = ssplit(moves, ' ');
  // now loop through
  for (int i = 0; i < (int)allMoves.size(); i++) {
    // make sure whitespaces and improper moves do not get through
    if (allMoves.at(i).length() < 4) continue;

    Move m = move_from(&pos, allMoves.at(i));
    // if MOVE_NONE then no legal move was found
    if (m == MOVE_NONE) return;
    // do the move now
    pos.do_move(m);
  }
  // doing the moves will increment the ply counter up which we don't wont
  // we want the ply to be 0 whenever a search is started, all the moves are still stored
  // for repetition sake however, purely an oversight in how the ply is incremented internally
  pos.set_ply(0);
}

void uci::stop() {
  sh.stop();
  if (mainThread.joinable()) {
    mainThread.join();
  }
}

void uci::quit() {
  uci::stop();
  TT.clear();
  TT.dealloc();
}

}
