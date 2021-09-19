# Rbowtie2
Bioconductor package: an R wrapper for Bowtie2 and AdapterRemoval 

The `Rbowtie2` package provides an R interface to the [bowtie2](https://github.com/BenLangmead/bowtie2) short read aligner by Langmead et al. (2009), and the [AdapterRemoval](https://github.com/MikkelSchubert/adapterremoval) package by Schubert, Lindgreen, and Orlando (2016). The `Rbowtie2` package allows users to remove adapter sequences from reads, build bowtie2 indexes (.bt2 or .bt2l), and to create bowtie2 alignment files (.sam or .bam). 

## Additional Installation Instructions

The package interfaces with the bowtie2 and bowtie2-build wrapper scripts provided in the bowtie2 v2.4.4 source code. The bowtie2 wrapper script is a `Perl` script and the bowtie2-build wrapper script is a `Python` script. Most versions of MacOS and Linux distributions come with a version of `Perl` and `Python` pre-installed. On Windows, both `Perl` and `Python` do not come pre-installed and they must be downloaded and installed manually. If either `Perl` or `Python` are not installed on your system follow the links below to download and install them.

Python: https://www.python.org/downloads/

Perl: https://www.perl.org/get.html

The package also uses `samtools` to create bam files if it is present on the system. The reason for this is explained under the **Bam File Creation** heading. However, `samtools` is completely optional and the package can be used without it. To download `samtools` follow the link below.

samtools: http://www.htslib.org/download/


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

