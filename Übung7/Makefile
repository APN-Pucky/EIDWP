# program name
PROGRAM = rechnung

# compiler
FC := gfortran

# compile
FCFLAGS = -Wall -Wextra -g -fbacktrace -pedantic -Wconversion
# link
FLFLAGS = 

reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))
# source files and objects
SRCS = $(call reverse ,$(patsubst %.f90, %.o, $(wildcard *.f90)))

all: $(PROGRAM)

$(PROGRAM): $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(PROGRAM)
