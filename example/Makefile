IRPF90= python ~/Work/Project/p16.03/irpf90/src/irpf90.py
IRPF90FLAGS=  -I ./ -I input/
BUILD_SYSTEM= ninja

export LD_LIBARY_PATH = 
export CXX = g++
export CXXFLAGS = -O2
export CC = gcc
export FC = gfortran
export CFLAGS = -O2
export FCFLAGS = -O2
export AR = AR


ifeq ($(BUILD_SYSTEM),ninja)
	CMD_PHONY=-t
	BUILD_FILE=IRPF90_temp/build.ninja
	IRPF90FLAGS += -j
else
	CMD_PHONY=
	BUILD_FILE=IRPF90_temp/build.make
endif


EXE := $(shell egrep -r '^\s*program' *.irp.f |  awk -v pwd="$$PWD" '{print pwd"/"$$2}')
.NOTPARALLEL: $(EXE)

.PHONY : all clean veryclean

all: $(BUILD_FILE)
	@echo 'BUILD $(EXE)'
	@$(BUILD_SYSTEM) -C $(dir $<) -f $(notdir $<) $(EXE) && touch $(EXE)

$(BUILD_FILE): $(shell find . -path ./IRPF90_temp -prune -o -name '*.irp.f' -maxdepth 2  -print)
	$(IRPF90) $(IRPF90FLAGS)

$(EXE): $(BUILD_FILE)
	$(BUILD_SYSTEM) -C $(dir $^) -f $(notdir $^) $@ && touch $@

clean: $(BUILD_FILE)
	$(BUILD_SYSTEM) -f $^ $(CMD_PHONY) clean
veryclean:
	rm -rf IRPF90_temp/ IRPF90_man/ irpf90_entities dist tags $(EXE)
