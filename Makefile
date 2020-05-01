tabulate: tabulate.o
	gfortran tabulate.o -static -s -o tabulate

tabulate.o: tabulate.f95
	gfortran -O2 -c tabulate.f95

install:
	tar -xvf direct.tar.gz
	mkdir csv
	mkdir tab
	mkdir reports
	./job.sh

clean:
	rm tabulate tabulate.o
