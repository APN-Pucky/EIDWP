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
TXTS = $(patsubst %.txt, %.pdf, $(wildcard *.txt))

#MOD = $(patsubst %.h, %.mo, $(wildcard *.h))

all: $(PROGRAM) 

zip: clean pdfs
	zip $(ZIPFILE) -R *

pdfs: $(TXTS)


$(PROGRAM): $(MOD) $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^

%.pdf: %.txt
	libreoffice --convert-to "pdf" $<
	#text2pdf $< #name .pdf => .txt.pdf

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

#%.mo: %.h
#	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.pdf *.o *.mod $(PROGRAM) $(ZIPFILE).zip
