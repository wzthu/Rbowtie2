## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----install, eval=FALSE------------------------------------------------------
#  BiocManager::install("Rbowtie2")

## ----loading------------------------------------------------------------------
library(Rbowtie2)

## ----idad---------------------------------------------------------------------
td <- tempdir()
reads_1 <- system.file(package="Rbowtie2", "extdata", "adrm", "reads_1.fq")
reads_2 <- system.file(package="Rbowtie2", "extdata", "adrm", "reads_2.fq")
(adapters <- 
    identify_adapters(file1=reads_1,file2=reads_2,
                      basename=file.path(td,"reads"),
                      "--threads 3",overwrite=TRUE))

## ----rmad---------------------------------------------------------------------
(cmdout<-remove_adapters(file1=reads_1,file2=reads_2,adapter1 = adapters[1], 
                adapter2 = adapters[2],
output1=file.path(td,"reads_1.trimmed.fq"),
output2=file.path(td,"reads_2.trimmed.fq"),
basename=file.path(td,"reads.base"),overwrite=TRUE,"--threads 3"))

## ----adrmusage----------------------------------------------------------------
adapterremoval_usage()

## ----adrmversion--------------------------------------------------------------
adapterremoval_version()

## ----bt2bd1-------------------------------------------------------------------
td <- tempdir()
refs <- dir(system.file(package="Rbowtie2", "extdata", "bt2","refs"),full=TRUE)
(cmdout<-bowtie2_build(references=refs, 
              bt2Index=file.path(td, "lambda_virus"), "--threads 4 --quiet",
              overwrite=TRUE))

## ----bt2bdusage---------------------------------------------------------------
bowtie2_build_usage()

## ----bt2align-----------------------------------------------------------------
reads_1 <- system.file(package="Rbowtie2", "extdata", 
                       "bt2", "reads", "reads_1.fastq")
reads_2 <- system.file(package="Rbowtie2", "extdata", 
                       "bt2", "reads", "reads_2.fastq")
if(file.exists(file.path(td, "lambda_virus.1.bt2"))){
    (cmdout<-bowtie2(bt2Index = file.path(td, "lambda_virus"),
        output = file.path(td, "result"),
        outputType = "sam",
        seq1=reads_1,
        seq2=reads_2,
        overwrite=TRUE,
        bamFile = NULL,
        "--threads 3"))
    head(readLines(file.path(td, "result.sam")))
}

## ----bt2usage-----------------------------------------------------------------
bowtie2_usage()

## ----bt2version---------------------------------------------------------------
bowtie2_version()

## ----sessioninfo--------------------------------------------------------------
sessionInfo()

