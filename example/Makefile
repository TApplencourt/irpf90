IRPF90= ../bin/irpf90
IRPF90FLAGS=  -I input/
# Make | Ninja
BUILD_SYSTEM= make

.EXPORT_ALL_VARIABLES:

LIB = 
SRC = 
OBJ = 

# Compiler ! Will be overwriten by the ENV one if avalaible.
FC ?= gfortran-6
FCFLAGS ?= -O2

CC ?= gcc
CFLAGS ?= -O2 
CXX ?= g++
CXXFLAGS ?= -O2

# Dark magic below modify with caution!
# "You are Not Expected to Understand This"
#                     .
#           /^\     .
#      /\   "V",
#     /__\   I      O  o
#    //..\\  I     .
#    \].`[/  I
#    /l\/j\  (]    .  O
#   /. ~~ ,\/I          .
#   \\L__j^\/I       o
#    \/--v}  I     o   .
#    |    |  I   _________
#    |    |  I c(`       ')o
#    |    l  I   \.     ,/
#  _/j  L l\_!  _//

#Misc
AR ?= 
RANLIB ?= 

# Variable need by IRPF90
ifeq ($(BUILD_SYSTEM),ninja)
        BUILD_FILE=IRPF90_temp/build.ninja
        IRPF90FLAGS += -j
else ifeq ($(BUILD_SYSTEM),make)
        BUILD_FILE=IRPF90_temp/build.make
        BUILD_SYSTEM += -j
else
DUMMY:
	$(error 'Wrong BUILD_SYSTEM: $(BUILD_SYSTEM)')
endif

# Actual Rule
EXE := $(shell egrep -ri '^\s*program' *.irp.f | cut -d'.' -f1)
ARCH = $(addprefix $(CURDIR)/,IRPF90_temp/irpf90.a)

.PHONY: clean all

all: $(EXE)

$(EXE): $(ARCH)
	@printf "%b" "\033[0;32m Build $@...\033[m\n"
	@$(BUILD_SYSTEM) -C $(dir $< ) -f $(notdir $(BUILD_FILE)) $(addprefix $(CURDIR)/, $@) && touch $@


.NOTPARALLEL: $(BUILD_FILE) $(ARCH)

$(ARCH): $(BUILD_FILE)
	@printf "%b" "\033[0;32m Creating the archive...\033[m\n"
	@$(BUILD_SYSTEM) -C $(dir $< ) -f $(notdir $< ) $(ARCH)

$(BUILD_FILE): $(shell find .  -maxdepth 2 -path ./IRPF90_temp -prune -o -name '*.irp.f' -print)
	@printf "%b" "\033[0;32m Running the IRPF90-compiler...\033[m\n"
	@$(IRPF90) $(IRPF90FLAGS)

clean:
	rm -f -- $(BUILD_FILE) $(EXE) 

veryclean: clean
	rm -rf IRPF90_temp/ IRPF90_man/ irpf90_entities dist tags

