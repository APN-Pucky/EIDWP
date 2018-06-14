# program name
PROGRAM = laender

# compiler
FC := gfortran

# compile
FCFLAGS = -Wall -Wextra -g -fbacktrace -pedantic -Wconversion
# link
FLFLAGS = 

# source files and objects
SRCS = $(patsubst %.f90, %.o, $(wildcard *.f90))

all: $(PROGRAM)

$(PROGRAM): $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.o
