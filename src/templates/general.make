IRPF90= irpf90
IRPF90FLAGS= {irpf90_flags}
# Make | Ninja
BUILD_SYSTEM= {BUILD_SYSTEM}

.EXPORT_ALL_VARIABLES:

LIB = {LIB}
SRC = {SRC}
OBJ = {OBJ}

# Compiler ! Will be overwriten by the ENV one if avalaible.
FC ?= {FC}
FCFLAGS ?= {FCFLAGS}

CC ?= {CC}
CFLAGS ?= {CFLAGS} 
CXX ?= {CXX}
CXXFLAGS ?= {CXXFLAGS}

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
AR ?= {ar}
RANLIB ?= {ranlib}

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

define run
	$(BUILD_SYSTEM) -C $(dir $(BUILD_FILE) ) -f $(notdir $(BUILD_FILE) ) $(1)
endef

#We allow for the user to ask for 'relative' path
#But the IRPF90 build system use absolute path.
$(EXE): $(ARCH)
	@printf "%b" "\033[0;32m Build $@...\033[m\n"
	@$(call run, $(addprefix $(CURDIR)/, $@)) && touch $@


.NOTPARALLEL: $(BUILD_FILE) $(ARCH)

$(ARCH): $(BUILD_FILE)
	@printf "%b" "\033[0;32m Creating the archive...\033[m\n"
	@$(call run, $@) && touch $@

$(BUILD_FILE): $(shell find .  -maxdepth 2 -path ./IRPF90_temp -prune -o -name '*.irp.f' -print)
	@printf "%b" "\033[0;32m Running the IRPF90-compiler...\033[m\n"
	@$(IRPF90) $(IRPF90FLAGS)

clean:
	rm -f -- $(BUILD_FILE) $(EXE) 

veryclean: clean
	rm -rf IRPF90_temp/ IRPF90_man/ irpf90_entities dist tags

