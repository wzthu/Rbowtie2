# Rbowtie2
Bioconductor package: an R wrapper for Bowtie2 and AdapterRemoval 

This package allows users to remove adapters from reads, build bowtie2 indexes (.bt2 or .bt2l), and to create bowtie2 alignment files (.sam or .bam). 
This package currently uses the asBam function of Rsamtools to create the .bam file. Unfortunately, this requires the .sam file to be created first 
which might be not viable for some users. Replacing Rsamtools with samtools can resolve this issue, but currrently there are issues with building the 
package with samtools on linux machines. This issue is currently under review.

The package uses bowtie2-2.4.4. To update the bowtie version used in the package, delete the current bowtie package in src directory and replace it with 
the current bowtie version. In the bowtie2 Makefile, all the .PHONY declarations can optionally be deleted besides clean. Change .PHONY clean to be ...


