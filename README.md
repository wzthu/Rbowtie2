# Rbowtie2
Bioconductor package: an R wrapper for Bowtie2 and AdapterRemoval 

The `Rbowtie2` package provides an R interface to the [bowtie2](https://github.com/BenLangmead/bowtie2) short read aligner by Langmead et al. (2009), and the [AdapterRemoval](https://github.com/MikkelSchubert/adapterremoval) package by Schubert, Lindgreen, and Orlando (2016). The `Rbowtie2` package allows users to remove adapter sequences from reads, build bowtie2 indexes (.bt2 or .bt2l), and to create bowtie2 alignment files (.sam or .bam). Note that creating a .bam requires that the corresponding .sam file first be created due to the asBam function of Rsamtools that is currently being used in the Rbowtie2 package. This might be not viable for some users. Replacing Rsamtools with samtools could potentially resolve this issue, and this README file will be updated if this change occurs.





The package uses bowtie2-2.4.4. To update the bowtie version used in the package, delete the current bowtie package in src directory and replace it with 
the current bowtie version. In the bowtie2 Makefile, all the .PHONY declarations can optionally be deleted besides .PHONY clean. Change .PHONY clean to be this


.PHONY: clean

clean:

	rm -f $(BOWTIE2_BIN_LIST) $(BOWTIE2_BIN_LIST_DBG) $(BOWTIE2_BIN_LIST_SAN) \
	$(addsuffix .exe,$(BOWTIE2_BIN_LIST) $(BOWTIE2_BIN_LIST_DBG)) \
	bowtie2-*.zip
	rm -f core.* .tmp.head
	rm -rf *.dSYM
	rm -rf .tmp
	rm -f ../../inst/bowtie2-align-s
	rm -f ../../inst/bowtie2-build-s
	rm -f ../../inst/bowtie2-align-l
	rm -f ../../inst/bowtie2-build-l
	rm -f bowtie2-align-s
	rm -f bowtie2-build-s
	rm -f bowtie2-align-l
	rm -f bowtie2-build-l
	rm -f *.o

Add a .PHONY move declaration to the bowtie2 Makefile and make it to be this

.PHONY: move

move: bowtie2-build-s bowtie2-align-s bowtie2-build-l bowtie2-align-l

	cp bowtie2-align-s ../../inst
	cp bowtie2-build-s ../../inst
	cp bowtie2-align-l ../../inst
	cp bowtie2-build-l ../../inst
  
  


