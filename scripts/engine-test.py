
import sys
import time
import subprocess
import os
import argparse
import math
import tqdm
import re
import atexit

# makes a folder, ensuring it does not exist first
def make_folder(path):
    if not os.path.exists(path):
        os.mkdir(path)

# finds a file from a given directory, defaulting to current directory
def find_file(name, path=os.getcwd()):
    for root, dirs, files in os.walk(path):
        if name in files:
            return os.path.join(root, name)

# checks if the environment is created and has all the dependencies
def check_path():
    if not os.path.isdir("Dependencies/OpeningBooks"):
        return True
    if not os.path.isdir("Output/PGN"):
        return True
    if not os.path.isdir("Output/Errors"):
        return True
    if not os.path.isdir("Output/Logs"):
        return True
    if not os.path.isdir("Output/Elo"):
        return True
    if not find_file("cutechess-cli"):
        return True
    if not find_file("UHO_XXL_+0.90_+1.19.epd"):
        return True
    if not find_file("ordo"):
        return True

# initializes the path and returns the opening book if it exists
def init_path():
    # make folders to store everything
    print("Running first Time setup, this may take a while ...")
    make_folder("Dependencies")
    make_folder("Output")
    make_folder("Output/Errors")
    make_folder("Output/PGN")
    make_folder("Output/Logs")
    make_folder("Output/Elo")
    make_folder("Dependencies/OpeningBooks")

    # run initialization and print updates
    print("Step 1/5. Cloning cutechess ...")
    subprocess.run(["git", "clone", CUTECHESS_GIT], cwd="Dependencies", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

    print("Step 2/5. Building cutechess ...")
    make_folder("Dependencies/cutechess/build")
    subprocess.run(["cmake", ".."], cwd="Dependencies/cutechess/build", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
    subprocess.run(["make", "-j", "cli"], cwd="Dependencies/cutechess/build", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

    print("Step 3/5. Cloning ordo ...")
    subprocess.run(["git", "clone", ORDO_GIT], cwd="Dependencies", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

    print("Step 4/5. Making ordo ...")
    subprocess.run(["make"], cwd="Dependencies/Ordo", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

    print("Step 5/5. Building opening book ...")
    subprocess.run(["wget", "-nc", OPENING_GIT], cwd="Dependencies/OpeningBooks", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
    subprocess.run(["unzip", "-o", "UHO_XXL_+0.90_+1.19.epd"], cwd="Dependencies/OpeningBooks", stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

# check for valid arg number and file exists
def setup_args():
    parser = argparse.ArgumentParser(
            prog="engine-test",
            description="Test one engine against a reference engine and determine which is better",
            usage="engine-test.py [-h] test-engine reference-engine [--threads] [--pgn] [--log]")

    parser.add_argument("reference_engine", help="the engine used as a reference", type=str)
    parser.add_argument("test_engines", help="the engines to be tested", type=str, nargs='+')
    parser.add_argument("--threads", help="number of threads to use for running games, will be limited by threads of processor",
                        type=int, default=1, metavar=f'[0-{os.cpu_count()}]')
    parser.add_argument("--nopgn", help="output games played to pgn files", action="store_true", required=False)
    parser.add_argument("--log", help="write output to log file", action="store_true", required=False)
    parser.add_argument("--debug", help="turn on engine debug info, does nothing if log is off", action="store_true", required=False)
    parser.add_argument("--noclean", help="do not clean all files after games are played", action="store_true", required=False)

    return parser

# clean all old pgn files and empty or old error files
def clean_path(deletion_window):
    current_time = time.time()
    pgn_files = os.listdir("Output/PGN")
    error_files = os.listdir("Output/Errors")

    # remove pgn files
    for file in pgn_files:
        if os.stat(f"{os.getcwd()}/Output/PGN/{file}").st_mtime < current_time - deletion_window:
            os.remove(f"{os.getcwd()}/Output/PGN/{file}")

    # remove error files
    for file in error_files:
        if os.stat(f"{os.getcwd()}/Output/Errors/{file}").st_mtime < current_time - deletion_window:
            os.remove(f"{os.getcwd()}/Output/Errors/{file}")

# run the command async and return the piped output
def async_exec(cmd, stderr):
    # run the isntance
    popen = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=stderr, universal_newlines=True)
    # setup python to close the instance in the event of a crash
    atexit.register(popen.terminate)
    # redout the lines
    for stdout_line in iter(popen.stdout.readline, ""):
        yield stdout_line
    popen.stdout.close()
    return_code = popen.wait()
    if return_code:
        raise subprocess.CalledProcessError(return_code, cmd)

# return a string to be printed for the elo difference
def elo_diff(p):
    if (p == 1):
        return "+inf"
    if (p == 0):
        return "-inf"
    diff = -400 * math.log10((1 - p) / p)
    if (diff == 0.5):
        return "+0"
    if (diff > 0.5):
        return "+{:.2f}".format(diff)
    else:
        return "{:.2f}".format(diff)

CUTECHESS_GIT = "https://github.com/cutechess/cutechess.git"
ORDO_GIT = "https://github.com/michiguel/Ordo.git"
OPENING_GIT = "https://github.com/official-stockfish/books/raw/master/UHO_XXL_+0.90_+1.19.epd.zip"

if __name__ == "__main__":
    # setup the arguments
    parser = setup_args()
    args = parser.parse_args()

    # check and setup path
    if check_path():
        init_path()

    # clean up old files
    clean_path(24 * 60 * 60)

    time_str = time.strftime("%Y%m%d-%H%M%S")

    m_nodes = 20000
    m_rounds = 500 # note this is two games per round
    m_stdout = subprocess.DEVNULL
    m_stderr = open(f"Output/Errors/{time_str}.error", "w")

    # construct an engine object for each test engine
    t_engines = []
    for engine in args.test_engines:
        t_engines += ["-engine", f"cmd={engine}"]

    t_args = ["Dependencies/cutechess/build/cutechess-cli",
            "-tournament", "gauntlet", "-engine", f"cmd={args.reference_engine}"]

    e_args = ["-each", "proto=uci", "option.Threads=1",
            "option.Hash=16", "tc=1/1", f"nodes={m_nodes}", "-rounds", f"{m_rounds}",
            "-recover", "-repeat","-games", "2", "-draw", "movenumber=40",
            "movecount=4", "score=8", "-resign", "movecount=4", "score=500",
            "-concurrency", f"{min(args.threads, os.cpu_count())}",
            "-openings", "file=Dependencies/OpeningBooks/UHO_XXL_+0.90_+1.19.epd",
            "format=epd", "order=random", "policy=round", "-maxmoves", "100"]

    m_args = t_args + t_engines + e_args

    # find name of reference engine
    r_name = None
    r_eng = async_exec([f"{args.reference_engine}", "uci", "exit"], m_stderr)
    for line in iter(r_eng):
        if "id name" in line:
            r_name = line[8:-1]

    # setup pgn output
    # if no reference name found will need to quit only if pgn on
    if (not args.nopgn):
        if r_name == None:
            exit("Name of reference engine could not be found, ensure uci protocol is correct")
        m_args.extend(["-pgnout", f"Output/PGN/{time_str}.pgn"])

    # setup a log file
    if (args.log):
        m_stdout = open(f"Output/Logs/{time_str}.log", "w")

    # setup debug mode
    if (args.debug):
        m_args.append("-debug")

    # play engines with low node limits
    print("Running games ...")
    with tqdm.tqdm(total=len(args.test_engines) * m_rounds * 2, smoothing=0, leave=False) as pbar:
        print_results = False
        for line in async_exec(m_args, m_stderr):
            # if the score update is in this line update the terminal window
            if "Finished game" in line:
                # update the progress bar with all the info
                pbar.update(1)

            # print cutechess elo if no pgn enabled
            if args.nopgn and ("Rank Name" in line or "Elo difference" in line):
                print_results = True
                pbar.close()

            if print_results:
                print(line, end="")

            # for all lines append them to a log file
            if args.log:
                m_stdout.write(line)

    # if we do not do standard elo printing can use ordo
    ordo_args = ["Dependencies/Ordo/ordo", "-Q", "-G", "-D", "-a", "0", "-A",
                f"{r_name}", "-W", "-n8", "-s1000", "-U", "0,1,2,3,4,5,6,7,8,9,10",
                "-j", f"Output/Elo/{time_str}.results", "-p", f"Output/PGN/{time_str}.pgn"]
    print_results = False
    for line in async_exec(ordo_args, m_stderr):
        if "# PLAYER" in line:
            print_results = True
        if print_results:
            print(line, end="")

    # close log and error file if open
    m_stderr.close()
    if (args.log):
        m_stdout.close()

    # check if error file is empty (usually is from cutechess)
    err_file = find_file(f"{time_str}.error")
    if err_file != None and os.stat(err_file).st_size == 0:
        os.remove(err_file)

    # if we can clean all the files
    if not args.noclean:
        err_file = find_file(f"{time_str}.error")
        pgn_file = find_file(f"{time_str}.pgn")
        log_file = find_file(f"{time_str}.log")
        if pgn_file != None:
            os.remove(pgn_file)
        if err_file != None:
            os.remove(err_file)
        if log_file != None:
            os.remove(log_file)
