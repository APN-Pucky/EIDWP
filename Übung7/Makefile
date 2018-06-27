# program name
PROGRAM = rechnung
ZIPFILE = Übung7

# compiler
FC := gfortran

# compile
FCFLAGS = -Wall -Wextra -g -fbacktrace -pedantic -Wconversion #-x f95 -std=f2008
# link
FLFLAGS =

#reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))
# source files and objects
SRCS = $(patsubst %.f90, %.o, $(wildcard *.f90))
#MOD = $(patsubst %.h, %.mo, $(wildcard *.h))

all: $(PROGRAM)

zip: clean
	zip $(ZIPFILE) -R *

$(PROGRAM): $(MOD) $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

#%.mo: %.h
#	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.o *.mod $(PROGRAM) $(ZIPFILE).zip
