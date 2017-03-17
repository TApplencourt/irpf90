IRPF90= ../bin/irpf90
IRPF90FLAGS=  
BUILD_SYSTEM= make

.EXPORT_ALL_VARIABLES:

AR = ar
CC = gcc
CFLAGS = -O2
CXX = g++
CXXFLAGS = -O2
FC = gfortran
FCFLAGS = -O2
LIB = 
RANLIB =  ranlib

OBJ = 
SRC = 

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
#  _/j  L l\_!  _//^---^\\_


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

define run_and_touch
        $(BUILD_SYSTEM) -C $(dir $(1) ) -f $(notdir $(1) ) $(addprefix $(CURDIR)/, $(2)) && touch $(2)
endef

EXE := $(shell egrep -ri '^\s*program' *.irp.f | cut -d'.' -f1)

.PHONY: all

all: $(BUILD_FILE)
	$(call run_and_touch, $<, $(EXE))

.NOTPARALLEL: $(EXE)
$(EXE): $(BUILD_FILE)
	$(call run_and_touch, $<, $(EXE))
$(BUILD_FILE): $(shell find .  -maxdepth 2 -path ./IRPF90_temp -prune -o -name '*.irp.f' -print)
	$(IRPF90) $(IRPF90FLAGS)

clean:
	rm -f -- $(BUILD_FILE) $(EXE)	$(shell find IRPF90_temp -type f \( -name "*.o" -o -name "*.mod" -name "*.a" \)  -delete;)
veryclean: clean
	rm -rf IRPF90_temp/ IRPF90_man/ irpf90_entities dist tags
