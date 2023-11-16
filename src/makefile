_THIS := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
_ROOT := $(_THIS)/..
_SRC := $(_ROOT)/src
_BIN := $(_ROOT)/bin
_MAKE := $(MAKE) --no-print-directory -C $(_THIS)

# Compiler and sources
CXX = g++
_LIBS_WL := -Wl,--whole-archive -lpthread -Wl,--no-whole-archive -fno-aggressive-loop-optimizations $(CFLAGS)
_LIBS := -pthread
_CXXSRCS := $(_SRC)/*.cpp $(_SRC)/nn/*.cpp
_CSRCS := $(_SRC)/syzygy/tbprobe.cpp
_SRCS := $(_CSRS) $(_CXXSRCS)

# Engine name and version
NAME = Luna
MINOR = 1
MAJOR = 2
MAKROS = -DMINOR_VERSION=$(MINOR) -DMAJOR_VERSION=$(MAJOR)

EVALFILE = $(_ROOT)/networks/luna.net

# Compilation flags
MARCH_FLAG = -march=native
PGO_PRE_FLAGS = -fprofile-generate -lgcov
PGO_POST_FLAGS = -fprofile-use -fno-peel-loops -fno-tracer

# COMPILATION OPTIONS #

# Options
NAMING ?= 1
VALGRIND ?= 1
STATIC ?= 0
NATIVE ?= 0
DETECT ?= 1
PGO ?= 0
DEBUG ?= 0
LTO ?= 0
PEXT ?= 0
# Vector instructions
AVX512 ?= 0
AVX2 ?= $(AVX512)
AVX ?= $(AVX2)
POPCNT ?= $(AVX)
SSE42 ?= $(POPCNT)
SSE41 ?= $(SSE42)
SSE3 ?= $(SSE41)
SSE2 ?= $(SSE3)
SSE ?= $(SSE2)

# HOST DETECTION #
ifeq ($(OS),Windows_NT)
	_LIBS += $(_LIBS_WL)
else
UNAME := $(shell uname -s)
ifeq ($(UNAME),Linux)
	_LIBS += $(_LIBS_WL)
else
ifeq ($(UNAME),Darwin)
	MARCH_FLAG := -mcpu=apple-a14
else
$(warning incompatible host, this might fail)
endif
endif
endif

# Disable all avx flags if native is set
ifeq ($(NATIVE),1)
	override AVX512 := *
	override AVX2   := *
	override AVX    := *
	override POPCNT := *
	override SSE42  := *
	override SSE41  := *
	override SSE3   := *
	override SSE2   := *
	override SSE    := *
endif

PROPS = $(shell echo | $(CC) $(MARCH_FLAG) -E -dM -)

# If DETECT is on, detect and enable flags
ifeq ($(DETECT),1)
	ifneq ($(findstring __BMI2__, $(PROPS)),)
		ifeq ($(findstring __znver1, $(PROPS)),)
			ifeq ($(findstring __znver2, $(PROPS)),)
				PEXT := 1
			endif
		endif
	endif
endif

# Disable PGO on mac systems and use NATIVE
ifeq ($(UNAME),Darwin)
	override PGO    := 0
	override STATIC := 0
	override NATIVE := 1
endif

# OUTPUT SPECIFICATION #

# Default EXE name and EXE directory
override DEFAULT_EXE_NAME := $(NAME)_$(MAJOR).$(MINOR)

# Specify EXE name and EXE dir
EXE_NAME := $(DEFAULT_EXE_NAME)
EXE_DIR  := $(_ROOT)/bin

# Build EXE name based on EXE dir and EXE name
EXE			:= $(EXE_DIR)/$(EXE_NAME)
EXE_DIR := $(dir $(abspath $(EXE)))

# BUILD FLAGS #

override FLAGS := -std=c++17 -Wall -Wextra -Wshadow -DEVALFILE=\"$(EVALFILE)\"
LDFLAGS += " -lgcov --coverage"

ifeq ($(VALGRIND),1)
	PEXT := 0
endif

ifeq ($(DEBUG),0)
	override FLAGS += -DNDEBUG -Ofast -ffloat-store -g
endif

ifeq ($(PEXT),1)
	override FLAGS += -DUSE_PEXT -mbmi2
endif

ifeq ($(LTO),1)
	override FLAGS += -flto
endif

ifeq ($(VALGRIND),1)
	override FLAGS += -lstdc++
endif

ifeq ($(STATIC),1)
	override FLAGS += -static -static-libgcc -static-libstdc++
endif

ifeq ($(NATIVE),1)
	override FLAGS += $(MARCH_FLAG)
endif

ifeq ($(AVX512),1)
	override FLAGS += -mavx512f -mavx512bw -mavx512dq
endif

ifeq ($(AVX2),1)
	override FLAGS += -mavx2
endif

ifeq ($(AVX),1)
	override FLAGS += -mavx
endif

ifeq ($(POPCNT),1)
	override FLAGS += -mpopcnt
endif

ifeq ($(SSE42),1)
	override FLAGS += -msse4.2
endif

ifeq ($(SSE41),1)
	override FLAGS += -msse4.1
endif

ifeq ($(SSE3),1)
	override FLAGS += -msse3
endif

ifeq ($(SSE2),1)
	override FLAGS += -msse2
endif

ifeq ($(SSE),1)
	override FLAGS += -msse
endif

# NAMING BASED ON FEATURES #
override EXE_INFO :=

# set name based on highest vector extension
ifeq ($(AVX512),1)
	override EXE_INFO := $(EXE_INFO)-avx512
else ifeq ($(AVX2),1)
	override EXE_INFO := $(EXE_INFO)-avx2
else ifeq ($(AVX),1)
	override EXE_INFO := $(EXE_INFO)-avx
else ifeq ($(POPCNT),1)
	override EXE_INFO := $(EXE_INFO)-popcnt
else ifeq ($(SSE42),1)
	override EXE_INFO := $(EXE_INFO)-sse42
else ifeq ($(SSE41),1)
	override EXE_INFO := $(EXE_INFO)-sse41
else ifeq ($(SSE3),1)
	override EXE_INFO := $(EXE_INFO)-msse3
else ifeq ($(SSE2),1)
	override EXE_INFO := $(EXE_INFO)-sse2
else ifeq ($(SSE),1)
	override EXE_INFO := $(EXE_INFO)-sse
endif

# pgo naming
ifeq ($(PGO),1)
	override EXE_INFO := $(EXE_INFO)-pgo
endif

# debug naming
ifeq ($(DEBUG),1)
	override EXE_INFO := $(EXE_INFO)-debug
endif

# pext naming
ifeq ($(PEXT),1)
	override EXE_INFO := $(EXE_INFO)-pext
endif

# OS naming
ifeq ($(OS),Windows_NT)
	override PREFIX := windows
	override SUFFIX := .exe
else
ifeq ($(UNAME),Linux)
	override PREFIX := linux
	override SUFFIX :=
else
ifeq ($(UNAME), Darwin)
	override PREFIX := darwin
	override SUFFIX :=
else
	override PREFIX :=
	override SUFFIX :=
endif
endif
endif

# Adjust the EXE path
ifeq ($(NAMING),1)
	EXE := $(EXE_DIR)/$(EXE_NAME)-$(PREFIX)$(EXE_INFO)$(SUFFIX)
endif

# Generate the EXE name which will be used for pgo
EXE_PGO := $(abspath $(EXE))

# RULES #

openbench:
	$(_MAKE) build DEBUG=0 NATIVE=1 PGO=1 LTO=1 DETECT=1 EVALFILE=$(EVALFILE) NAMING=0 EXE=$(EXE)

native:
	$(_MAKE) build DEBUG=0 NATIVE=1 PGO=0 LTO=1 DETECT=1 NAMING=$(NAMING) STATIC=1 EXE_NAME=$(EXE_NAME)

pgo:
	$(_MAKE) build DEBUG=0 NATIVE=1 PGO=1 LTO=1 DETECT=1 NAMING=$(NAMING) STATIC=1 EXE_NAME=$(EXE_NAME)

valgrind:
	$(_MAKE) build DEBUG=0 NATIVE=0 PGO=0 LTO=1 DETECT=1 NAMING=$(NAMING) STATIC=0 VALGRIND=1 EXE_NAME=$(EXE_NAME)

debug:
	$(_MAKE) build DEBUG=1 NATIVE=1 PGO=0 LTO=1 DETECT=1 NAMING=$(NAMING) STATIC=1 EXE_NAME=$(EXE_NAME)

prebuild:
	$(_MAKE) prebuild DEBUG=0 NATIVE=1 PGO=0 LTO=1 DETECT=1 STATIC=1

overview:
	$(info NAME      : $(NAME))
	$(info EXE       : $(EXE))
	$(info EXE_NAME  : $(EXE_NAME))
	$(info EXE_DIR   : $(EXE_DIR))
	$(info NAMING    : $(NAMING))
	$(info NATIVE    : $(NATIVE))
	$(info LTO       : $(LTO))
	$(info STATIC    : $(STATIC))
	$(info PEXT      : $(PEXT))
	$(info PGO       : $(PGO))
	$(info DEBUG     : $(DEBUG))
	$(info AVX512    : $(AVX512))
	$(info AVX2      : $(AVX2))
	$(info AVX       : $(AVX))
	$(info POPCNT    : $(POPCNT))
	$(info SSE42     : $(SSE42))
	$(info SSE41     : $(SSE41))
	$(info SSE3      : $(SSE3))
	$(info SSE2      : $(SSE2))
	$(info SSE       : $(SSE))

build: updateNetwork overview
	mkdir -p $(EXE_DIR)
    ifeq ($(PGO),1)
		$(CXX) $(PGO_PRE_FLAGS) $(FLAGS) $(_SRCS) $(MAKROS) $(_LIBS) -o $(EXE)
		$(EXE_PGO) bench
		$(CXX) $(PGO_POST_FLAGS) $(FLAGS) $(_SRCS) $(MAKROS) $(_LIBS) -o $(EXE)
		@rm -f $(EXE_DIR)*.gcda
    else
		$(CXX) $(FLAGS) $(_SRCS) $(MAKROS) $(_LIBS) -o $(EXE)
    endif

prebuild: updateNetwork overview
	$(CXX) $(FLAGS) $(_SRCS) $(MAKROS) $(_LIBS) -c

release:
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 AVX512=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 AVX2=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 POPCNT=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE42=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE41=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE3=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE2=1
	$(_MAKE) build DEBUG=0 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE=1

	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 AVX512=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 AVX2=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 POPCNT=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE42=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE41=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE3=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE2=1
	$(_MAKE) build DEBUG=0 PEXT=1 PGO=1 LTO=1 DETECT=0 NAMING=1 STATIC=1 SSE=1

# Update the network
updateNetwork:
