#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. General
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Find the OS
UNAME := $(shell uname -s)
ifeq ($(UNAME), Linux)
	OS := $(shell uname -o)
endif

ifeq ($(OS), Windows_NT)
	windows = yes
endif

# File name
NAME = luna
MAJOR = 2
MINOR = 2

# Source code directory
ROOT := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SRCS := $(ROOT)/*.cpp $(ROOT)/nn/*.cpp
OBJS := $(notdir $(SRCS:.cpp=.o))

# Default network name
EVALFILE := $(ROOT)/../networks/luna-00000000.nn

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Configuration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# option                | flag        	| description
#==============================================================================
#													|
# debug = yes/no				| -DNDEBUG 			| enable/disable debugging mode
# optimize = yes/no				| -0fast			| enable compiler optimizations
# popcnt = yes/no				| -DUSE_POPCNT		| use popcnt asm-instruction
# pext = yes/no					| -DUSE_PEXT		| use pext asm-instruction
# sse = yes/no					|	-msse			| use Intel Streaming SIMD extensions
# sse2 = yes/no					|	-msse2			| use Intel Streaming SIMD extensions 2
# sse3 = yes/no					| -msse3			| use Intel Streaming SIMD extensions 3
# sse41 = yes/no				| -msse4.1			| use Intel Streaming SIMD extensions 4.1
# sse42 = yes/no				| -msse4.2			| use Intel Streaming SIMD extensions 4.2
# avx2 = yes/no					| -mavx2			| use Intel Advanced Vector Instructions 2
# avx512 = yes/no				| -mavx512bw		| use Intel Advanced Vector Instructions 512
#==============================================================================

# 2.1 compilation options
debug = no
optimize = yes
popcnt = no
pext = no
sse = no
sse2 = no
sse3 = no
sse41 = no
sse42 = no
avx2 = no
avx512 = no

CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wcast-qual -fno-exceptions -pedantic -Wextra -Wshadow \
						-fno-aggressive-loop-optimizations -DMINOR_VERSION=$(MINOR) \
						-DMAJOR_VERSION=$(MAJOR) -DEVALFILE=\"$(EVALFILE)\"
PREPGOFLAGS = -fprofile-generate -lgcov
POSTPGOFLAGS = -fprofile-use -fno-peel-loops -fno-tracer -lgcov

# 2.2 OS specific flags use pthread on windows and lpthread on linux
ifeq ($(OS), Windows_NT)
	CXXFLAGS += -static -pthread
else
ifeq ($(UNAME), Linux)
	CXXFLAGS += -lpthread -static -static-libgcc -static-libstdc++
endif
endif

# 2.3 Arch specific flags
ifeq ($(ARCH),)
	ARCH = native
	CXXFLAGS += -march=native
endif

ifeq ($(findstring x86-64, $(ARCH)), x86-64)
	sse2 = yes
endif

ifeq ($(findstring -sse3, $(ARCH)), -sse3)
	sse = yes
	sse2 = yes
	sse3 = yes
endif

ifeq ($(findstring -popcnt, $(ARCH)), -popcnt)
	popcnt = yes
endif

ifeq ($(findstring -sse41, $(ARCH)), -sse3)
	sse = yes
	sse2 = yes
	sse3 = yes
	sse41 = yes
endif

ifeq ($(findstring -sse42, $(ARCH)), -sse3)
	sse = yes
	sse2 = yes
	sse3 = yes
	sse41 = yes
	sse42 = yes
endif

ifeq ($(findstring -bmi2, $(ARCH)), -bmi2)
	popcnt = yes
	sse = yes
	sse2 = yes
	sse3 = yes
	sse41 = yes
	sse42 = yes
	avx2 = yes
	pext = yes
endif

ifeq ($(findstring -avx, $(ARCH)), -avx)
	popcnt = yes
	sse = yes
	sse2 = yes
	sse3 = yes
	sse41 = yes
	sse42 = yes
endif

ifeq ($(findstring -avx2, $(ARCH)), -avx2)
	popcnt = yes
	sse = yes
	sse2 = yes
	sse3 = yes
	sse41 = yes
	sse42 = yes
	avx2 = yes
endif

ifeq ($(findstring -avx512, $(ARCH)), -avx512)
	popcnt = yes
	sse = yes
	sse2 = yes
	sse3 = yes
	sse41 = yes
	sse42 = yes
	avx2 = yes
	pext = yes
	avx512 = yes
endif

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Low-Level Configuration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 3.1 debugging information
ifeq ($(debug), yes)
	CXXFLAGS += -g
else
	CXXFLAGS += -DNDEBUG
endif

# 3.2 optimization settings
ifeq ($(optimize), yes)
	CXXFLAGS += -Ofast -ffloat-store
endif

# 3.3 SIMD and vector instructions
ifeq ($(pext), yes)
	CXXFLAGS += -DUSE_PEXT -mbmi2
endif


ifeq ($(avx512), yes)
	CXXFLAGS += -mavx512f -mavx512bw
endif

ifeq ($(avx2), yes)
	CXXFLAGS += -mavx2 -mbmi
endif

ifeq ($(popcnt), yes)
	CXXFLAGS += -DUSE_POPCNT -mpopcnt
endif

ifeq ($(sse42), yes)
	CXXFLAGS += -msse4.2
endif

ifeq ($(sse41), yes)
	CXXFLAGS += -msse4.1
endif

ifeq ($(sse3), yes)
	CXXFLAGS += -msse3
endif

ifeq ($(sse2), yes)
	CXXFLAGS += -msse2
endif

ifeq ($(sse), yes)
	CXXFLAGS += -msse
endif

# 3.4.1 Include git sha for version
GIT_SHA := $(shell git rev-parse HEAD 2>/dev/null | cut -c 1-8)
ifneq ($(GIT_SHA), )
	CXXFLAGS += -DGIT_SHA=$(GIT_SHA)
endif

# 3.4.2 Include the git commit date for version
GIT_DATE := $(shell git show -s --date=format:'%Y%m%d' --format=%cd HEAD 2>/dev/null)
ifneq ($(GIT_DATE), )
	CXXFLAGS += -DGIT_DATE=$(GIT_DATE)
endif

# 3.5 Link optimization for lto
ifeq ($(optimize), yes)
ifeq ($(debug), no)
	CXXFLAGS += -flto
endif
endif

# 3.6 EXE name
PREFIX = linux
SUFFIX =

ifeq ($(OS), Windows_NT)
	override PREFIX = windows
	override SUFFIX = .exe
endif

EXE_DIR := $(ROOT)/../bin
EXE := $(EXE_DIR)/$(NAME)-$(MAJOR).$(MINOR)-$(PREFIX)-$(ARCH)$(SUFFIX)
EXE_PGO := $(abspath $(EXE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Compilation Targets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

help:
	@echo ""
	@echo "To compile luna, type: "
	@echo ""
	@echo "make target [ARCH=arch]"
	@echo ""
	@echo "Targets: "
	@echo "help                         > Displays information on targets"
	@echo "pgo                          > Creates a build with profile guided optimization"
	@echo "build                        > Standard build without profile guided optimization"
	@echo "net                          > Requires you to be connected to the internet; downloads the neural net"
	@echo "clean                        > Clean up the directory"
	@echo ""
	@echo "Supported arch: "
	@echo "native                       > This is used by default and selects the best architecture for your processor"
	@echo "x86-64-avx512                > 64-bit with avx512 support"
	@echo "x86-64-avx2                  > 64-bit with avx2 support"
	@echo "x86-64-bmi2                  > 64-bit with bmi2 support"
	@echo "x86-64-sse42-popcnt          > 64-bit with sse4.2 and popcnt support"
	@echo "x86-64-sse41-popcnt          > 64-bit with sse4.1 and popcnt support"
	@echo "x86-64-sse3-popcnt           > 64-bit with sse3 and popcnt support"
	@echo "x86-64-sse3                  > 64-bit with sse3 support"
	@echo "x86-64                       > 64-bit generic build with sse2 support"
	@echo ""
	@echo "Examples: "
	@echo "make pgo ARCH=x86-64-avx2    > Creates a fast and modern build with profile guided optimization"
	@echo "make build ARCH=x86-64       > Creates a highly compatible build skipping profile guided optimization"
	@echo ""

build: net
	@mkdir -p $(EXE_DIR)
	$(CXX) $(CXXFLAGS) $(SRCS) -o $(EXE) > /dev/null 2>&1

pgo: net obj-clean pgo-clean
	@echo ""
	@echo "Step 1/4. Building initial executable ..."
	@mkdir -p $(EXE_DIR)
	$(CXX) $(PREPGOFLAGS) $(CXXFLAGS) $(SRCS) -o $(EXE) > /dev/null 2>&1
	@echo ""
	@echo "Step 2/4. Running benchmark ..."
	$(EXE_PGO) bench > PGOBENCH.out 2>&1
	tail -n 1 PGOBENCH.out
	@echo ""
	@echo "Step 3/4. Building optimized executable ..."
	$(MAKE) ARCH=$(ARCH) obj-clean
	$(CXX) $(POSTPGOFLAGS) $(CXXFLAGS) $(SRCS) -o $(EXE) > /dev/null 2>&1
	@echo ""
	@echo "Step 4/4. Cleaning up ..."
	$(MAKE) ARCH=$(ARCH) pgo-clean

obj-clean:
	@rm -f *.o /nn/*.o

pgo-clean:
	@rm -rf pgodir
	@rm -f bench.txt ../bin/*.gcda ../bin/*.gcno PGOBENCH.out

default:
	help

net:
