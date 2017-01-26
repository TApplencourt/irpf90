PYTHON=python
PYVERSION=$(shell $(PYTHON) -c "import sys; print(sys.version[:3])")
../bin/irpf90: irpf90_python.exe
	rm ../bin/irpf90 ; cd ../bin ; ln -s ../src/irpf90_python.exe irpf90

clean:
	rm -f *.c *.so *.pyc *.pyo 2>/dev/null
