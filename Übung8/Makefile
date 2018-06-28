# program name
PROGRAM = generic_program_name.out
ZIPFILE = $(shell basename $(shell pwd))

# compiler
FC := gfortran

# compile
FCFLAGS = -Wall -Wextra -g -fbacktrace -pedantic -Wconversion #-x f95 -std=f2008
# link
FLFLAGS =

#reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))
# source files and objects
SRC_O = $(patsubst %.f90, %.o, $(wildcard *.f90))
SRC_OUT = $(patsubst %.f90, %.out, $(wildcard *.f90))
TXTS = $(patsubst %.txt, %.pdf, $(wildcard *.txt))

#MOD = $(patsubst %.h, %.mo, $(wildcard *.h))

$(PROGRAM): $(SRC_O) # make sinle.out from all .f90
	$(FC) $(FLFLAGS) -o $@ $^

all: $(SRC_OUT) #make single .f90 to single .out

zip: clean pdfs
	zip $(ZIPFILE) -R *

pdfs: $(TXTS)


%.pdf: %.txt
	libreoffice --convert-to "pdf" $<
	#text2pdf $< #name .pdf => .txt.pdf

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

%.out: %.f90
	$(FC) $(FCFLAGS) -o $@ $<


#%.mo: %.h
#	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.pdf *.o *.mod $(PROGRAM) $(ZIPFILE).zip
