CXX ?= clang++
CXXWARN ?= -Werror -Wall -Wextra -Wno-unused -pedantic

CXXSTD ?= c++17
SRC=src/main.cpp

CXXFLAGS+=-fno-exceptions -fno-rtti

BUILD_DIR=build
TARGET=klx

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
	CXXFLAGS+=-Og -g -march=native -fno-omit-frame-pointer
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

