# pragma once

# include "types.h"
# include "bitboard.h"
# include "attacks.h"
# include "position.h"
# include "search.h"
# include "misc.h"
# include "timer.h"
# include "nn/evaluator.h"

# include <stdio.h>
# include <cstdlib>
# include <iostream>
# include <string>
# include <thread>
# include <vector>

namespace Luna {

namespace uci {

// main function that will loop
void loop(int argc, char* argv[]);

// function to process arguments sent in from the terminal
void parse(std::string args, Position& pos);

// parse go commands
void parse_go(std::string args, Position& pos);

// parse position commands
void parse_position(std::string args, Position& pos);

// parse setoption commands
void set_option(std::string option, std::string val);

// stop the search
void stop();

// send uci info to the gui
void uci();

// eval function to return a fancy eval of the position
void eval();

// bench function to run a benchmark, useful for compiler options
void bench();

// quits the program
void quit();
}
}
