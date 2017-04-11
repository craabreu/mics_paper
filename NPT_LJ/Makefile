FORT  = gfortran
FOPTS = -O3 -march=native -ffast-math -funroll-loops -fstrict-aliasing -cpp -Wunused

CC = g++
COPTS = -O3 -funroll-loops -fstrict-aliasing -Wall -W -Wno-uninitialized

all: NPT_LJ

clean:
	rm -rf *.o
	rm -rf *.mod
	rm -rf NPT_LJ

NPT_LJ: NPT_LJ.o mUtil.o mTypes.o
	$(FORT) $(FOPTS) -o $@ $^

NPT_LJ.o: NPT_LJ.f90 mUtil.o mTypes.o
	$(FORT) $(FOPTS) -c -o $@ $<

mUtil.o: mUtil.f90 mTypes.o
	$(FORT) $(FOPTS) -c -o $@ $<

mTypes.o: mTypes.f90
	$(FORT) $(FOPTS) -c -o $@ $<

