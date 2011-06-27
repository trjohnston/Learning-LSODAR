FC=gfortran
FFLAGS=-g -O0 -Wall

all : lsodartest2

opkda1.o : opkda1.f
opkda2.o : opkda2.f
opkdmain.o : opkdmain.f
lsodartest2.o : lsodartest2.f

lsodartest2 : opkda1.o opkda2.o opkdmain.o lsodartest2.o
	$(FC) $(FFLAGS) opkda1.o opkda2.o opkdmain.o lsodartest2.o -o lsodartest2

clean :
	rm -rf *.o lsodartest2

.PHONY : all clean
