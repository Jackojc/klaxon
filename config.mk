CXX ?= clang++
CXXWARN ?= -Werror -Wall -Wextra -Wno-unused -pedantic -Wno-unused-parameter

CXXSTD ?= c++17

SRC_KLX=src/main.cpp
SRC_OPT=src/opt.cpp
SRC_CFG=src/cfg.cpp

BUILD_DIR=build

TARGET_KLX=klx
TARGET_OPT=opt
TARGET_CFG=cfg

# Libraries to include and link
INC=-Iinc/ -Isrc/
LIBS=$(LDLIBS)

# Flags
dbg ?= yes
san ?= no

# Debug flags
ifeq ($(dbg),no)
	CXXFLAGS+=-O3 -march=native -flto -DNDEBUG -s
else ifeq ($(dbg),yes)
	CXXFLAGS+=-g -fno-omit-frame-pointer
else
$(error dbg should be either yes or no)
endif

# Sanitizer flags
ifeq ($(san),yes)
	CXXFLAGS+=-fsanitize=undefined,leak
else ifeq ($(san),no)

else
$(error san should be either yes or no)
endif

