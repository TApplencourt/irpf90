IRPF90 = python ../src/irpf90.py -I input -a -d -j
FC     = ifort 
FCFLAGS= -O2
NINJA  = ninja -j 1 -v

SRC=
OBJ=
LIB=

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
