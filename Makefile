# klaxon

.POSIX:

include config.mk

all: options compiler optimiser

config:
	@mkdir -p $(BUILD_DIR)

options:
	@printf "cc \033[32m$(CXX)\033[0m | "
	@printf "dbg \033[32m$(dbg)\033[0m | "
	@printf "san \033[32m$(san)\033[0m | "
	@printf "cflags \033[32m-std=$(CXXSTD) $(CXXFLAGS)\033[0m\n"

compiler: config
	@printf "cc main.cpp\n"
	@$(CXX) -std=$(CXXSTD) $(CXXWARN) $(CXXFLAGS) $(LDFLAGS) $(CPPFLAGS) $(INC) \
		$(LIBS) -o $(BUILD_DIR)/$(TARGET_KLX) $(SRC_KLX)

optimiser: config
	@printf "cc opt.cpp\n"
	@$(CXX) -std=$(CXXSTD) $(CXXWARN) $(CXXFLAGS) $(LDFLAGS) $(CPPFLAGS) $(INC) \
		$(LIBS) -o $(BUILD_DIR)/$(TARGET_OPT) $(SRC_OPT)

clean:
	rm -rf $(BUILD_DIR)/ *.gcda

.PHONY: all options clean

