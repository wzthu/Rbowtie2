# Rbowtie2
Bioconductor package: an R wrapper for Bowtie2 and AdapterRemoval 

The `Rbowtie2` package provides an R interface to the [bowtie2](https://github.com/BenLangmead/bowtie2) short read aligner by Langmead et al. (2009), and the [AdapterRemoval](https://github.com/MikkelSchubert/adapterremoval) package by Schubert, Lindgreen, and Orlando (2016). The `Rbowtie2` package allows users to remove adapter sequences from reads, build bowtie2 indexes (.bt2 or .bt2l), and to create bowtie2 alignment files (.sam or .bam). Note that creating a .bam requires that the corresponding .sam file first be created due to the asBam function of Rsamtools that is currently being used in the `Rbowtie2` package. This might be not viable for some users. Replacing Rsamtools with samtools could potentially resolve this issue, and this README file will be updated if this change occurs.


## Source Package Information
The `Rbowtie2` package uses the bowtie2 v2.4.4 source code which was obtained from https://sourceforge.net/projects/bowtie-bio/. The folders doc, example, scripts, and some non-code files were deleted to reduce the package size. To manually update the bowtie2 source package, delete the current bowtie2 folder in the src directory and replace it with the current bowtie2 release version. In the src/bowtie2 directory there should be a Makefile. Open the Makefile and make the following changes:

1. Update the .PHONY clean declaration 
```
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
	rm -f ../../inst/bowtie2
	rm -f ../../inst/bowtie2-build
	rm -f bowtie2-align-s
	rm -f bowtie2-build-s
	rm -f bowtie2-align-l
	rm -f bowtie2-build-l
```
2. Add a .PHONY clean_dSYM declaration (Primarily to delete large (>5Mb) files after package installation)
```
.PHONY: clean_dSYM
clean_dSYM:
	rm -rf *.dSYM
```

3. Add a .PHONY move declaration 
```
.PHONY: move
move: bowtie2-build-s bowtie2-align-s bowtie2-build-l bowtie2-align-l bowtie2 bowtie2-build

	cp bowtie2-align-s ../../inst
	cp bowtie2-build-s ../../inst
	cp bowtie2-align-l ../../inst
	cp bowtie2-build-l ../../inst
	cp bowtie2 ../../inst
	cp bowtie2-build ../../inst
```

In the Rbowtie2/src directory there should be two Makefiles (Makefile or Makefile.win). Depending on OS version, open the corresponding Makefile and change the BT2_DIR variable to be name of the bowtie directory.

