#Makefile adapted from (has been modified)
#http://stackoverflow.com/questions/5871780/creating-a-fortran-makefile

#Compiler
#-fbackslash is for using backslash like in C
#-g is for using gdb
#-ffree-line-length-0 is to allow arbitrarily large line lengths
#-ffpe-trap is to trap floating point exceptions 
FC = gfortran -fbackslash -g -ffree-line-length-0 -ffpe-trap=invalid,zero,overflow

#source files, use the most dependent in the end
SRCS = $(addsuffix .o, gnuplot_fortran lorenz2)

PROGRAM = lorenz

all: $(PROGRAM)

$(PROGRAM): $(SRCS)
	$(FC) -o $@ $^

%.o : %.f95
	$(FC) -o $@ -c $<

clean:
	rm -f *.o *.mod
