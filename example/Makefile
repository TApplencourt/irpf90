IRPF90 = python ../src/irpf90.py -I input -a -d 
FC     = ifort 
FCFLAGS= -O2
NINJA  = 

SRC=
OBJ=
LIB=

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
