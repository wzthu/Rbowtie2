#' @name bowtie2
#' @title Interface to bowtie2 using bowtie2-2.4.4
#' @description This function can be use to call the wrapped \code{bowtie2-align-s}
#'  or \code{bowtie2-align-l} binary.
#' @importFrom magrittr %>%
#' @param bt2Index \code{Character} scalar. Path to bowtie2 index files including
#' index prefix: dir/basename
#' (minus trailing .bt2 or .bt2l of dir/basename.*.bt2 or dir/basename.*.bt2l).
#' @param outputPath \code{Character} scalar. A path to the alignment output file
#' (dir/basename) (minus trailing .sam or .bam).
#' @param outputType \code{Character} scalar. Specifies the type of output alignment file
#' to be created. Default is set to "sam" but user can also specify "bam" instead. 
#' @param seq1 \code{Character} vector. For single-end sequencing,
#' it contains sequence file paths.
#' For paired-end sequencing, it can be file paths with #1 mates
#' paired with file paths in seq2.
#' And it can also be interleaved file paths when argument
#' interleaved=\code{TRUE}
#' @param seq2 \code{Character} vector. It contains file paths with
#' #2 mates paired with file paths in seq1.
#' For single-end sequencing files and interleaved paired-end
#' sequencing files(argument interleaved=\code{TRUE}),
#' it must be \code{NULL}.
#' @param bamFile \code{Character} vector. File path to bam file that contains
#' unaligned reads.
#' @param ... Additional arguments to be passed on to the binaries.
#' See below for details.
#' @param interleaved \code{Logical}. Set \code{TRUE} when files are
#' interleaved paired-end sequencing data.
#' @param overwrite \code{Logical}. Force overwriting of existing
#' files if setting \code{TRUE}.
#' @details All additional arguments in ... are interpreted as
#' additional parameters to be passed to 
#' bowtie2. All of them should be \code{Character} or
#' \code{Numeric} scalar. You can put all aditional
#' arguments in one \code{Character} (e.g. "--threads 8 --no-mixed")
#' with white space splited just like command line,
#' or put them in different \code{Character}
#' (e.g. "--threads","8","--no-mixed"). Note that some
#' arguments("-x","--interleaved","-U","-1","-2","-S") to the
#' bowtie2 are invalid if they are already handled as explicit
#' function arguments. See the output of
#' \code{bowtie2_usage()} for details about available parameters.
#' @author Zheng Wei, Rahul Varki
#' @return An invisible \code{Integer} of call
#' status. The value is 0 when there is not any mistakes
#' Otherwise the value is non-zero.
#' @references Langmead, B., & Salzberg, S. L. (2012).
#' Fast gapped-read alignment with Bowtie 2. Nature methods, 9(4), 357-359.
#' @export bowtie2
#' @examples
#' td <- tempdir()
#' ## Building a bowtie2 index
#' refs <- dir(system.file(package="Rbowtie2", "extdata", "bt2","refs"),
#' full=TRUE)
#' bowtie2_build(references=refs, bt2Index=file.path(td, "lambda_virus"),
#' "--threads 4 --quiet",overwrite=TRUE)
#' ## Alignments
#' reads_1 <- system.file(package="Rbowtie2", "extdata", "bt2", "reads",
#' "reads_1.fastq")
#' reads_2 <- system.file(package="Rbowtie2", "extdata", "bt2", "reads",
#' "reads_2.fastq")
#' if(file.exists(file.path(td, "lambda_virus.1.bt2"))){
#'     cmdout<-bowtie2(bt2Index = file.path(td, "lambda_virus"),
#'        samOutput = file.path(td, "result.sam"),
#'        seq1=reads_1,seq2=reads_2,overwrite=TRUE,"--threads 3");cmdout
#'     head(readLines(file.path(td, "result.sam")))
#' }
#'

bowtie2 <- function(bt2Index,outputPath,outputType = "sam", seq1=NULL,..., seq2=NULL,bamFile=NULL,
                    interleaved=FALSE, overwrite=FALSE){
    if(R.Version()$arch=="i386"){
        return("bowtie2 is not available for 32bit, please use 64bit R instead")
    }
    
    if (outputType != "sam" && outputType != "bam"){
        stop(paste0("Specify either 'sam' or 'bam' for outputType input"))
    }
    
    
    # Convert local path into absolute path for debugging purposes
    bt2Index <- file.path(tools::file_path_as_absolute(dirname(bt2Index)), basename(bt2Index))
    bt2Index <-trimws(as.character(bt2Index))
    samOutput <- file.path(tools::file_path_as_absolute(dirname(outputPath)), paste0(basename(outputPath),".sam"))
    samOutput<-trimws(as.character(samOutput))

    if (!is.null(seq1))
        seq1<-trimws(as.character(seq1))
    else if (!is.null(bamFile))
        bamFile <- trimws(as.character(bamFile))
    
    
    # If paired mates are provided then they should be the same length
    if(!is.null(seq2)){
        seq2<-trimws(as.character(seq2))
        if(length(seq1)!=length(seq2)){
            stop(paste0("The lengths of arguments ",
                        "`seq1` and `seq2` should be the same length"))
        }
    }
    paramArray<-checkAddArgus("-x|--interleaved|-U|-1|-2|-b|-S",...)


    if(interleaved){
        if(length(seq1)>1){
            stop(paste0("Argument `seq1` has to be a SINGLE file",
                        " path rather than a vector of paths"))
        }
        if(!is.null(seq2)){
            stop("Argument `seq2` has to be NULL when interleaved=TRUE")
        }
    }

    # Check if files and paths provided actually exist otherwise warning appears
    checkFileExist(seq1,"seq1")
    checkFileExist(seq2,"seq2")
    checkPathExist(bt2Index,"bt2Index")
    
    # Detect whether index at path provided is small (.bt2), large (.bt2l), or non-existent
    indexFormat <- checkIndexType(bt2Index)
    
    if (indexFormat == "SMALL"){
        checkFileExist(paste0(bt2Index,".1.bt2"),"bt2Index")
        checkFileExist(paste0(bt2Index,".2.bt2"),"bt2Index")
        checkFileExist(paste0(bt2Index,".3.bt2"),"bt2Index")
        checkFileExist(paste0(bt2Index,".4.bt2"),"bt2Index")
        checkFileExist(paste0(bt2Index,".rev.1.bt2"),"bt2Index")
        checkFileExist(paste0(bt2Index,".rev.2.bt2"),"bt2Index")
    }
    
    else if (indexFormat == "LARGE"){
        checkFileExist(paste0(bt2Index,".1.bt2l"),"bt2Index")
        checkFileExist(paste0(bt2Index,".2.bt2l"),"bt2Index")
        checkFileExist(paste0(bt2Index,".3.bt2l"),"bt2Index")
        checkFileExist(paste0(bt2Index,".4.bt2l"),"bt2Index")
        checkFileExist(paste0(bt2Index,".rev.1.bt2l"),"bt2Index")
        checkFileExist(paste0(bt2Index,".rev.2.bt2l"),"bt2Index")
    }
    
    else{
        stop(paste0("Could not find either a valid small (.bt2) or large (.bt2l)", 
                    " index with basename of ",basename(bt2Index), " at location ",
                    dirname(bt2Index)))
    }
    
    checkFileCreatable(samOutput,"samOutput",overwrite)

    # Create the explicit arguments for the binaries
    argvs = c("-x",bt2Index)

    if(!is.null(seq1) && is.null(seq2)){
        if(interleaved){
            seq1<-paste0(seq1,collapse = ",")
            argvs <- c(argvs,"--interleaved",seq1)
        }else{
            seq1<-paste0(seq1,collapse = ",")
            argvs <- c(argvs,"-U",seq1)
        }
    }else if(!is.null(seq1) && !is.null(seq2)){
        seq1<-paste0(seq1,collapse = ",")
        seq2<-paste0(seq2,collapse = ",")
        argvs <- c(argvs,"-1",seq1,"-2",seq2)
    }
    else if (!is.null(bamFile)){
        bamFile <- paste0(bamFile, collapse = ",")
        argvs <- c(argvs,"-b",bamFile)
    }
    else{
        stop(paste0("Either could not find input reads or bam file"))
    }

    # Combine explicit and optional arguments together
    argvs <- c(paramArray,argvs,"-S",samOutput)

    # Call corresponding bowtie-align binary depending on index type and output type
    if (indexFormat == "SMALL"){
        if(outputType == "sam"){
            invisible(.callbinary("bowtie2-align-s",paste(argvs,collapse = " ")))
        }
        else if (outputType == "bam"){
            invisible(.callbinary("bowtie2-align-s",paste(argvs,collapse = " "), path = samOutput)
                      %>%
                          Rsamtools::asBam(file = ., 
                                           destination = file.path(tools::file_path_as_absolute(dirname(outputPath)), basename(outputPath)),
                                           overwrite = overwrite, 
                                           indexDestination = FALSE))
            
            invisible(file.remove(samOutput))
            
        }
        else{
            stop("Bug exists that needs to be fixed")
        }
    }
    else if (indexFormat == "LARGE"){
        if(outputType == "sam"){
            invisible(.callbinary("bowtie2-align-l",paste(argvs,collapse = " ")))
        }
        else if (outputType == "bam"){
            invisible(.callbinary("bowtie2-align-l",paste(argvs,collapse = " "), path = samOutput)
                      %>%
                          Rsamtools::asBam(file = ., 
                                           destination = file.path(tools::file_path_as_absolute(dirname(outputPath)), basename(outputPath)),
                                           overwrite = overwrite, 
                                           indexDestination = FALSE))
            
            invisible(file.remove(samOutput))
        }
        else{
            stop("Bug exists that needs to be fixed")
        }
    }
        
    else
        stop("Bug exists that needs to be fixed")

}



#' @name bowtie2-build
#' @title Interface to bowtie2-build using bowtie2-2.4.4
#' @description This function can be use to call the wrapped \code{bowtie2-build-s}
#'  or \code{bowtie2-build-l} binary
#' @param references \code{Character} vector. The path to the files containing
#' the references for which to
#' build a bowtie index.
#' @param bt2Index \code{Character} scalar. Write bowtie2 index data to files
#' with this prefix: 'dir/basename'.
#' If the files with path like dir/basename.*.bt2 or dir/basename.*.bt2l already exists,
#' the function function will cast an error,
#' unless argument overwrite is \code{TRUE}.
#' @param ... Additional arguments to be passed on to the binaries.
#' See below for details.
#' @param overwrite \code{Logical}. Force overwriting of existing files
#' if setting \code{TRUE}.
#' @details All additional arguments in ... are interpreted as additional
#' parameters to be passed on to
#' bowtie2_build. All of them should be \code{Character} or
#' \code{Numeric} scalar. You can put all additional
#' arguments in one \code{Character} (e.g. "--threads 8 --quiet") with white
#' space splited just like command line,
#' or put them in different \code{Character} (e.g. "--threads","8","--quiet").
#' See the output of
#' \code{bowtie2_build_usage()} for details about available parameters.
#' @author Zheng Wei, Rahul Varki
#' @return An invisible \code{Integer} of call status.
#' The value is 0 when there is not any mistakes
#' Otherwise the value is non-zero.
#' @references Langmead, B., & Salzberg, S. L. (2012). Fast gapped-read
#' alignment with Bowtie 2. Nature methods, 9(4), 357-359.
#' @export bowtie2_build
#' @examples
#' td <- tempdir()
#' ## Building a bowtie2 index
#' refs <- dir(system.file(package="Rbowtie2", "extdata", "bt2","refs"),
#' full=TRUE)
#' cmdout<-bowtie2_build(references=refs, bt2Index=file.path(td, "lambda_virus"),
#' "--threads 4 --quiet",overwrite=TRUE);cmdout
#' ## Use additional arguments in another way
#' cmdout<-bowtie2_build(references=refs, bt2Index=file.path(td, "lambda_virus"),
#' "--threads",4,"--quiet",overwrite=TRUE);cmdout
#' ## The function will print the output
#' ## during the process without "--quiet" argument.
#' cmdout<-bowtie2_build(references=refs, bt2Index=file.path(td, "lambda_virus"),
#' overwrite=TRUE);cmdout

bowtie2_build <- function(references,bt2Index,...,overwrite=FALSE){
    if(R.Version()$arch=="i386"){
        return("bowtie2 is not available for 32bit, please use 64bit R instead")
    }
    references<- trimws(as.character(references))
    bt2Index <- trimws(as.character(bt2Index))

    paramArray<-checkAddArgus("noinvalid",...)

    # Check if reference file exists and path to place the bowtie indexes exist
    checkFileExist(references,"references")
    checkPathExist(bt2Index,"bt2Index")
    
    # Check whether to build small (.bt2) or large (.bt2l) indexes
    # Same calculation used as the bowtie wrapper
    
    delta = 200
    smallIndex_max_size = 4*1024**3 - delta
    
    total_size = 0
    for (filename in references){
        total_size = total_size + file.size(filename)
    }
    
    # If the total file size of the reference files is larger than 
    # the max small index size then we create large indexes (.bt2l)
    if (total_size > smallIndex_max_size){
        checkFileCreatable(paste0(bt2Index,".1.bt2l"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".2.bt2l"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".3.bt2l"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".4.bt2l"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".rev.1.bt2l"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".rev.2.bt2l"),"bt2Index",overwrite)
    }
    
    # If not then we create small indexes (.bt2)
    else{
        
        checkFileCreatable(paste0(bt2Index,".1.bt2"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".2.bt2"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".3.bt2"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".4.bt2"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".rev.1.bt2"),"bt2Index",overwrite)
        checkFileCreatable(paste0(bt2Index,".rev.2.bt2"),"bt2Index",overwrite)
    }
    
    # Combine explicit and optional arguments together
    references<-paste0(references,collapse = ",")
    argvs <- c(paramArray,references,bt2Index)

    # Call corresponding bowtie-build binary depending on index type
    if (total_size > smallIndex_max_size)
        invisible(.callbinary("bowtie2-build-l",paste(argvs,collapse = " ")))
    else
        invisible(.callbinary("bowtie2-build-s",paste(argvs,collapse = " ")))
}

#' @name bowtie2_version
#' @title Print version information of bowtie2-2.4.4
#' @description Print version information of bowtie2-2.4.4
#' @author Zheng Wei, Rahul Varki
#' @param indexType \code{Character} scalar. Defaults to 's' which shows version
#' number of the small bowtie2-build and bowtie-align binaries. 
#' Can optionally pass 'l' which shows version number of the large bowtie2-build
#' and bowtie2-align binaries.
#' @return An invisible \code{Integer} of call status.
#' The value is 0 when there is not any mistakes
#' Otherwise the value is non-zero.
#' @references Langmead, B., & Salzberg, S. L. (2012). Fast gapped-read
#' alignment with Bowtie 2. Nature methods, 9(4), 357-359.
#' @export bowtie2_version
#' @examples
#' cmdout<-bowtie2_version();cmdout
bowtie2_version <- function(indexType = 's'){
    if(R.Version()$arch=="i386"){
        return("bowtie2 is not available for 32bit, please use 64bit R instead")
    }
    
    if (indexType == 'l'){
        #.callbinary("bowtie2-build-l","--version")
        .callbinary("bowtie2-align-l","--version")
    }
    else{
        #.callbinary("bowtie2-build-s","--version")
        .callbinary("bowtie2-align-s","--version")
    }
}

#' @name bowtie2_usage
#' @title Print available arguments for bowtie2
#' @description Note that some arguments to the
#' bowtie2 are invalid if they are
#' already handled as explicit function arguments.
#' @author Zheng Wei, Rahul Varki
#' @param indexType \code{Character} scalar. Defaults to 's' which shows help
#' document of the small bowtie2-align binary. 
#' Can optionally pass 'l' which shows help document of the large 
#' bowtie2-align binary.
#' @return bowtie2 available arguments and their usage.
#' @references Langmead, B., & Salzberg, S. L. (2012). Fast gapped-read
#' alignment with Bowtie 2. Nature methods, 9(4), 357-359.
#' @export bowtie2_usage
#' @examples
#' bowtie2_usage()
bowtie2_usage <- function(indexType = 's'){
    if(R.Version()$arch=="i386"){
        return("bowtie2 is not available for 32bit, please use 64bit R instead")
    }
    
    if (indexType == 'l')
        .callbinary("bowtie2-align-l","-h")
    else
        .callbinary("bowtie2-align-s","-h")
}

#' @name bowtie2_build_usage
#' @title Print available arguments for bowtie2_build_usage
#' @description Note that some arguments to the
#' bowtie2_build_usage are invalid if they are
#' already handled as explicit function arguments.
#' @author Zheng Wei, Rahul Varki
#' @param indexType \code{Character} scalar. Defaults to 's' which shows help
#' document of the small bowtie2-build binary. 
#' Can optionally pass 'l' which shows help document of the large 
#' bowtie2-build binary
#' @return bowtie2_build available arguments and their usage.
#' @references Langmead B, Salzberg S.
#' Fast gapped-read alignment with Bowtie 2. Nature Methods. 2012, 9:357-359.
#' @export bowtie2_build_usage
#' @examples
#' bowtie2_build_usage()
bowtie2_build_usage <- function(indexType = 's') {
    if(R.Version()$arch=="i386"){
        return("bowtie2 is not available for 32bit, please use 64bit R instead")
    }
    
    if (indexType == 'l')
        .callbinary("bowtie2-build-l","-h")
    else
        .callbinary("bowtie2-build-s","-h")
}


