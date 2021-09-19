# Rbowtie2
Bioconductor package: an R wrapper for Bowtie2 and AdapterRemoval 

The `Rbowtie2` package provides an R interface to the [bowtie2](https://github.com/BenLangmead/bowtie2) short read aligner by Langmead et al. (2009), and the [AdapterRemoval](https://github.com/MikkelSchubert/adapterremoval) package by Schubert, Lindgreen, and Orlando (2016). The `Rbowtie2` package allows users to remove adapter sequences from reads, build bowtie2 indexes (.bt2 or .bt2l), and to create bowtie2 alignment files (.sam or .bam). 

## Bam File Creation
The `Rbowtie2` package attempts to use `samtools` to create the bam file if present on the system. This method of creating the bam file avoids the need to create an intermediate sam file which might be infeasable depending on the size of the file and the available memory on the system. If `samtools` cannot be found on the system, the package attempts to create the bam file via `Rsamtools` which requires that an intermediate sam file be created prior to the bam file being created.


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

